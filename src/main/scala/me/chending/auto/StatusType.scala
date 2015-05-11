package me.chending.auto

import scala.annotation.tailrec
import scala.collection.{Set, immutable, mutable}
import scala.language.experimental.macros
import scala.language.postfixOps
import scala.reflect.runtime.universe._

case class StatusType[T](name: String)(implicit tag: TypeTag[T]) {

    /**
     * OnAcceptInputHandler(acceptedInput, fromStatus, toStatus)
     */
    type OnAcceptInputHandler = (T, StatusType[T], StatusType[T]) => Unit

    case class Intention(handler: OnAcceptInputHandler, target: StatusType[T])

    private val _relations = mutable.Map[T, Intention]()
    private var defaultIntention: Intention = null

    def relations = immutable.Map(_relations.toSeq: _*)

    def accept(input: Set[T]) = new Bridge(input)

    def accept(input: T) = new Bridge(input)

    def acceptDefaultWithHandler = new Bridge().withHandler _

    def acceptableInputs: Set[T] = _relations.keySet

    def acceptType = tag.tpe

    def onAccept(input: T): StatusType[T] = {
        input match {
            case i if acceptableInputs.contains(i) =>
                relations(i) match {
                    case Intention(handler, target) =>
                        handler(i, this, target)
                        target
                }
            case _ =>
                defaultIntention match {
                    case Intention(null, target) => target
                    case Intention(handler, target) =>
                        handler(input, this, target)
                        target
                    case _ => null
                }
        }
    }

    class Bridge(inputs: Set[T]) {
        def this(input: T) = this(Set(input))

        def this() = this(null)

        private var onAcceptInputHandler: OnAcceptInputHandler = null

        def withHandler(handler: OnAcceptInputHandler): Bridge = {
            onAcceptInputHandler = handler
            this
        }

        def goto(target: StatusType[T]): StatusType[T] = {
            if (inputs == null) {
                StatusType.this.defaultIntention = Intention(onAcceptInputHandler, target)
            } else {
                inputs foreach {
                    input =>
                        _relations += input -> Intention(onAcceptInputHandler, target)
                }
            }
            StatusType.this
        }
    }

}

object Status {
    type Type[T] = StatusType[T]

    private val statusMap = mutable.Map[String, StatusType[_]]()

    private def getStatusByName[T: TypeTag](name: String): StatusType[T] = {
        val status = statusMap.getOrElseUpdate(name, StatusType[T](name))
        status.acceptType match {
            case t if t =:= typeOf[T] => status.asInstanceOf[StatusType[T]]
            case _ => val st = StatusType[T](name)
                this.statusMap.update(name, StatusType[T](name))
                st
        }

    }

    def apply[T: TypeTag](name: String): StatusType[T] = getStatusByName[T](name)

}

object StatusUtility {
    def printTree[T](root: StatusType[T]): Unit = {
        @tailrec
        def _internalPrint(toPrint: List[StatusType[T]], printed: List[StatusType[T]]): Unit = {
            if (toPrint.isEmpty) {
                return
            }
            val status = toPrint.head
            val unprinted = status.relations.values filter {
                intention => !printed.contains(intention.target)
            } map {
                intention => intention.target
            } toList

            if (!printed.contains(status)) {
                println("Status: " + status.name + " accepts:")
                status.relations foreach {
                    elem: (T, status.Intention) => elem._2 match {
                        case status.Intention(handler, target) =>
                            println("input: `" + elem._1 + "` go to status: " + target.name)
                    }
                }
            }
            _internalPrint(unprinted ::: toPrint.tail, status :: printed)
        }

        _internalPrint(root :: Nil, List[StatusType[T]]())
    }

}