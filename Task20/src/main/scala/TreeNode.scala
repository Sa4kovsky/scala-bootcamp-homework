import Operation.{Contains, Insert}
import TreeNode._

import akka.actor.{Actor, Props}
import akka.cluster.pubsub.DistributedPubSubMediator.Remove

object TreeNode {
  case object Left
  case object Right

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props[TreeNode](elem, initiallyRemoved)
}

final class TreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  override def receive: Receive = {
    case insert: Insert => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove: Remove => doRemove(remove)
  }

  def doInsert(ins: Insert) =
    if(elem > ins.elem)
      {
        ???
      }
    else if(elem < ins.elem)
      {
        ???
      }
    else
      {
        ???
      }

  def doContains(cont: Contains) = ???

  def doRemove(rem: Remove) = ???

}