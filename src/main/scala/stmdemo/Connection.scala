package stmdemo

trait Connection {
  def sendMessage(message: Message): Unit
  def close(): Unit
}
