import akka.actor.{Actor, ActorRef}

import Operation._

trait Operation
object Operation{
  final case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation
  final case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation
  final case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation
}

object TreeSet extends Actor{

  private def createRoot: ActorRef = context.actorOf(TreeNode.props(20, initiallyRemoved = true))

  override def receive: Receive = {
    case message: Operation => createRoot ! message
  }
}