package stmdemo

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.stm._
import scala.concurrent.stm.Txn._

/** This is an STM implementation of the Router trait. Clients and
  * connections are tracked using immutable Maps inside Refs. The
  * connection map is extended with my ImmutableMultiMapExtension to
  * provide an immutable 1-many mapping of connections to clients.
  */
class STMRouter extends Router {
  private val clients = Ref(Map.empty[ClientId, Client])
  private val connections = Ref(Map.empty[Connection, Set[Client]])
  private val rollbacks = new AtomicInteger(0)

  def route(message: Message) = {
    getClient(message.dest) match {
      case Some(c) => c.queueMessage(message)
      case None => // Send unknown client response
    }
  }

  import ImmutableMultiMapExtension._

  def addClient(client: Client): Unit =
    atomic { implicit txn =>
      Txn.afterRollback(rollbackHandler)
      connections transform (_ addBinding (client.connection, client))
      clients transform (_ updated (client.id, client))
    }

  def removeClient(client: Client): Unit =
    atomic { implicit txn =>
      Txn.afterRollback(rollbackHandler)
      connections transform (_ removeBinding (client.connection, client))
      clients transform (_ - client.id)
    }

  private val clientsView = clients.single
  def getClient(clientId: ClientId): Option[Client] = clientsView() get (clientId)

  private val connectionsView = connections.single
  def getClients(connection: Connection): Set[Client] = connectionsView() getOrElse (connection, Set.empty[Client])

  def removeConnection(connection: Connection): Set[Client] =
    atomic { implicit txn =>
      Txn.afterRollback(rollbackHandler)
      val cs = getClients(connection)
      cs foreach { c => removeClient(c) }
      cs
    }

  def rollbackHandler(status: Status) = rollbacks.incrementAndGet
  def rollbackCount = rollbacks.get
}
