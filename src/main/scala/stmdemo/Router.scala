package stmdemo

/** This defines the interface to a very simple message router.  A
  * connection represents a physical network connection. A connection
  * multiplexes potentially several clients. Removing a connection
  * must also remove all associated clients.  A client is both a
  * source and a sink of messages.  Clients are addressed by their
  * client id.
  */
trait Router {
  def route(message: Message): Unit

  def addClient(client: Client): Unit
  def removeClient(client: Client): Unit

  def getClient(clientId: ClientId): Option[Client]
  def getClients(connection: Connection): Set[Client]

  def removeConnection(connection: Connection): Set[Client]
}
