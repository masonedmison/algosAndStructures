package queue

import cats.collections.Dequeue

/** Implementing in terms of cats-collection `Dequeue` as a start
  */

case class Queue[+A] private (underlying: Dequeue[A]) {
  def push[B >: A](e: B) = underlying.cons(e)
  def pop: Option[(A, Queue[A])] =
    underlying.unsnoc.map { case (a, deq) =>
      a -> new Queue(deq)
    }
  def toList: List[A] = underlying.toList
}

object Queue {
  def apply[A](elements: A*) = new Queue(Dequeue(elements))
}
