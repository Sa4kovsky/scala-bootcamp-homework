import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.cluster.pubsub.DistributedPubSubMediator.Remove

import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps

sealed  trait OperationTree
object OperationTree{
  final case class Insert(requester: ActorRef, id: Int, elem: Int) extends OperationTree
  final case class Contains(requester: ActorRef, id: Int, elem: Int) extends OperationTree
  final case class Delete(requester: ActorRef, id: Int, elem: Int) extends OperationTree
}

sealed trait TypeOfReply
object TypeOfReply {
  final case class ContainsResult(id: Int, result: Boolean) extends TypeOfReply
  final case class TheEnd(id: Int) extends TypeOfReply
}

object  ActorModel extends App {

  final class  TreeSet extends Actor {

    import OperationTree._
    import TypeOfReply._

    private def createRoot: ActorRef = context.actorOf(TreeNode.props(10, initiallyRemoved = true))

    override def receive: Receive = {
      case "Start" =>
        (1 to 5).map(i => Insert(createRoot, i, Random.between(5, 15))).foreach { insert =>
          println(s"Send: $insert")
          createRoot ! insert
        }

      case TheEnd(id) =>
        println(s"Operation with id = $id finished")
      case ContainsResult(id, result) =>
        println(s"Contains result with id = $id is $result")
    }
  }

  val treeActorSystem: ActorSystem = ActorSystem("tree-actor-system")
  val mainRef: ActorRef = treeActorSystem.actorOf(Props[TreeSet](), "main")
  mainRef ! "Start"
}