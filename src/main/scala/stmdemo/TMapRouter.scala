package stmdemo

import scala.concurrent.stm._

/** An implementation of the Router trait using TMap. Clients and
  * connections are tracked using mutable TMaps. The connection map
  * is extended with my ImmutableMultiMapExtension to provide an
  * mutable, transactional 1-many mapping of connections to clients.
  */
class TMapRouter extends Router {
  private val clients = TMap[ClientId, Client]()
  private val connections = TMap[Connection, Set[Client]]()

  def route(message: Message) = {
    getClient(message.dest) match {
      case Some(c) => c.queueMessage(message)
      case None => // Send unknown client response
    }
  }

  import ImmutableMultiMapExtension._

  def addClient(client: Client): Unit =
    atomic { implicit txn =>
      connections addBinding(client.connection, client)
      clients += ((client.id, client))
    }

  def removeClient(client: Client): Unit =
    atomic { implicit txn =>
      connections removeBinding (client.connection, client)
      clients -= client.id
    }

  val clientView = clients.single
  def getClient(clientId: ClientId): Option[Client] = clientView.get(clientId)

  val connectionView = connections.single
  def getClients(connection: Connection): Set[Client] = connectionView.get(connection).getOrElse(Set.empty[Client])

  def removeConnection(connection: Connection): Set[Client] =
    atomic { implicit txn =>
      val cs = getClients(connection)
      cs foreach { c => removeClient(c) }
      cs
    }
}
