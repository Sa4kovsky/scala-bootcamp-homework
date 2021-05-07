import OperationTree._
import TreeNode._
import TypeOfReply.{ContainsResult, TheEnd}
import akka.actor.{Actor, ActorRef, Props}

import javax.swing.text.Position
import scala.util.control.TailCalls.TailRec

trait Position
case object Left extends Position
case object Right extends Position

object TreeNode {
  import Position._

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new TreeNode(elem, initiallyRemoved))
}

final class TreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  var state = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  override def receive: Receive = {
    case insert: Insert => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove: Delete => doRemove(remove)
  }

  def selectPosition(newElem: Int): Option[ActorRef] = if (newElem < elem) state.get(Left) else state.get(Right)

  def doInsert(ins: Insert) =
    if(elem > ins.elem)
      {
       state.get(Left).fold {
          state = state + (Left -> context.actorOf(props(ins.elem, initiallyRemoved = false)))
        }(_ ! ins)
        sender() ! TheEnd(ins.id)
      }
    else if(elem < ins.elem)
      {
        state.get(Right).fold {
          state = state + (Right -> context.actorOf(props(ins.elem, initiallyRemoved = false)))
        }(_ ! ins)
        sender() ! TheEnd(ins.id)
      }
    else
      {
        removed = false
        sender() ! TheEnd(ins.id)
      }

  def doContains(cont: Contains) = {
    val position: Option[ActorRef] = selectPosition(cont.elem)
    if (cont.elem == elem && !removed) sender() ! ContainsResult(cont.id, result = true)
    else position.fold(sender() ! ContainsResult(cont.id, result = false))(_ ! cont)
  }

  def doRemove(rem: Delete) = {
    val position: Option[ActorRef] = selectPosition(rem.elem)
    if (rem.elem == elem) {
      removed = true
      rem.requester ! ContainsResult(rem.id, removed)
    } else
      position.fold(rem.requester ! TheEnd(rem.id))(_ ! rem)
  }

}