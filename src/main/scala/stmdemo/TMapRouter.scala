package stmdemo

import scala.concurrent.stm._

class TMapRouter extends Router {
  private val clients = TMap[ClientId, Client]()
  private val connections = TMap[Connection, Set[Client]]()

  def addBinding[A, B](m: TMap[A, Set[B]], a: A, b: B)(implicit txn: InTxn): Unit = {
    m += ((a, m.get(a).map(s => s + b).getOrElse(Set(b))))
  }

  def removeBinding[A, B](m: TMap[A, Set[B]], a: A, b: B)(implicit txn: InTxn): Unit = {
    val set = m.get(a) map { s => s - b } getOrElse { Set.empty[B] }
    if (set.isEmpty) m.remove(a)
    else m += ((a, set))
  }

  def route(message: Message) = {
    getClient(message.dest) match {
      case Some(c) => c.queueMessage(message)
      case None => // Send unknown client response
    }
  }

  import ImmutableMultiMapExtension._

  def addClient(client: Client): Unit =
    atomic { implicit txn =>
      addBinding(connections, client.connection, client)
      clients += ((client.id, client))
    }

  def removeClient(client: Client): Unit =
    atomic { implicit txn =>
      removeBinding (connections, client.connection, client)
      clients -= client.id
    }

  def getClient(clientId: ClientId): Option[Client] = clients.single.get(clientId)

  def getClients(connection: Connection): Set[Client] = connections.single.get(connection).getOrElse(Set.empty[Client])

  def removeConnection(connection: Connection): Set[Client] =
    atomic { implicit txn =>
      val cs = getClients(connection)
      cs foreach { c => removeClient(c) }
      cs
    }
}
