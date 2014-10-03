package stmdemo

/** A bad implementation of the Router trait. This version provides
  * no concurrency protection at all. It is intended only to demonstrate
  * what happens when we ignore concurrency.
  */
class BadRouter extends Router {
  private var clients = Map.empty[ClientId, Client]
  private var connections = Map.empty[Connection, Set[Client]]

  def route(message: Message) = {
    getClient(message.dest) match {
      case Some(c) => c.queueMessage(message)
      case None    => // Send unknown client response
    }
  }

  import ImmutableMultiMapExtension._

  def addClient(client: Client): Unit = {
    connections = connections addBinding (client.connection, client)
    clients = clients updated (client.id, client)
  }

  def removeClient(client: Client): Unit = {
    connections = connections removeBinding (client.connection, client)
    clients = clients - client.id
  }

  def getClient(clientId: ClientId): Option[Client] = clients get (clientId)

  def getClients(connection: Connection): Set[Client] = connections getOrElse (connection, Set.empty[Client])

  def removeConnection(connection: Connection): Set[Client] = {
    val cs = getClients(connection)
    cs foreach { c => removeClient(c) }
    cs
  }
}
