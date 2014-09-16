package stmdemo

trait Client {
  def id: ClientId
  def name: String

  def connection: Connection

  def queueMessage(message: Message): Unit
  def disconnect(): Unit
}
