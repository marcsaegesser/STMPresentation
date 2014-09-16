package stmdemo

import scala.concurrent.stm._

class STMRouter extends Router {
  private val clients = Ref(Map.empty[ClientId, Client])
  private val connections = Ref(Map.empty[Connection, Set[Client]])

  def route(message: Message) = {
    getClient(message.dest) match {
      case Some(c) => c.queueMessage(message)
      case None => // Send unknown client response
    }
  }

  import ImmutableMultiMapExtension._

  def addClient(client: Client): Unit =
    atomic { implicit txn =>
      connections transform (_ addBinding (client.connection, client))
      clients transform (_ updated (client.id, client))
    }

  def removeClient(client: Client): Unit =
    atomic { implicit txn =>
      connections transform (_ removeBinding (client.connection, client))
      clients transform (_ - client.id)
    }

  private val clientsView = clients.single
  def getClient(clientId: ClientId): Option[Client] = clientsView() get (clientId)

  private val connectionsView = connections.single
  def getClients(connection: Connection): Set[Client] = connectionsView() getOrElse (connection, Set.empty[Client])

  def removeConnection(connection: Connection): Set[Client] =
    atomic { implicit txn =>
      val cs = getClients(connection)
      cs foreach { c => removeClient(c) }
      cs
    }
}
