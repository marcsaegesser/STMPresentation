package stmdemo

/** An implementation of the Router trait using conventional lock
  * objects and synchronization. Clients and connections are tracked
  * using immutable Maps. The connection map is extended with my
  * ImmutableMultiMapExtension to provide an immutable 1-many mapping
  * of connections to clients.
  */
class SyncRouter extends Router {
  private var clients = Map.empty[ClientId, Client]
  private var connections = Map.empty[Connection, Set[Client]]
  private val lockObject = new Object

  def route(message: Message) = {
    getClient(message.dest) match {
      case Some(c) => c.queueMessage(message)
      case None => // Send unknown client response
    }
  }

  import ImmutableMultiMapExtension._

  def addClient(client: Client): Unit =
    lockObject.synchronized{
      connections = connections addBinding (client.connection, client)
      clients = clients updated (client.id, client)
    }

  def removeClient(client: Client): Unit =
    lockObject.synchronized{
      connections = connections removeBinding (client.connection, client)
      clients = clients - client.id
    }

  def getClient(clientId: ClientId): Option[Client] = clients get (clientId)

  def getClients(connection: Connection): Set[Client] = connections getOrElse (connection, Set.empty[Client])

  def removeConnection(connection: Connection): Set[Client] =
    lockObject.synchronized{
      val cs = getClients(connection)
      cs foreach { c => removeClient(c) }
      cs
    }
}
