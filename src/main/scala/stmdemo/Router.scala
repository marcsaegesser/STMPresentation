package stmdemo

trait Router {
  def route(message: Message): Unit

  def addClient(client: Client): Unit
  def removeClient(client: Client): Unit

  def getClient(clientId: ClientId): Option[Client]
  def getClients(connection: Connection): Set[Client]

  def removeConnection(connection: Connection): Set[Client]
}
