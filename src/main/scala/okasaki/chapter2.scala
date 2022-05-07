package okasaki

import cats.Order
import cats.implicits._
import okasaki.chapter2.Tree.Branch

object chapter2 {

  sealed trait Tree[+A]

  object Tree {
    case object E extends Tree[Nothing]
    case class Branch[A](left: Tree[A], elem: A, right: Tree[A]) extends Tree[A]
  }

  trait Set[A, S[_]] {
    def empty: S[A]
    def insert: A => S[A] => S[A]
    def member: A => S[A] => Boolean
  }

  class UnbalancedSet[A: Order]() extends Set[A, Tree] {

    override def empty: Tree[A] = Tree.E

    override def insert: A => Tree[A] => Tree[A] = v => {
      case Tree.Branch(l, e, r) =>
        if (v < e)
          Tree.Branch(insert(v)(l), v, r)
        else
          Tree.Branch(l, v, insert(v)(r))
      case Tree.E =>
        Tree.Branch(Tree.E, v, Tree.E)
    }

    override def member: A => Tree[A] => Boolean = v => {
      case Tree.Branch(l, e, r) =>
        if (v < e) member(e)(l)
        else if (v > e)
          member(e)(r)
        else
          true

      case Tree.E => false
    }

    /** ex 2.3 A member that only makes d + 1 comparisons
      */
    def memberV2: A => A => Tree[A] => Boolean = last =>
      v => {
        case Tree.Branch(l, e, r) =>
          if (v < e) memberV2(last)(v)(l)
          else
            memberV2(e)(v)(r)

        case Tree.E =>
          if (last === v) true
          else false
      }

    /** ex 2.5
      */
    def complete: A => Int => Tree[A] = v =>
      n => {
        // only will occur if 0 is the initial passed in value
        if (n == 0) Tree.E
        if (n == 1) Tree.Branch(Tree.E, v, Tree.E)
        else {
          val shared = complete(v)(n - 1)
          Tree.Branch(shared, v, shared)
        }
      }

    def create2: A => Int => (Tree[A], Tree[A]) = v =>
      m => {
        create(v)(m + 1) -> create(v)(m)
      }

    def create: A => Int => Tree[A] = v =>
      m => {
        if (m == 0) Tree.E
        else if (m == 1) Tree.Branch(Tree.E, v, Tree.E)
        else if ((m - 1) % 2 == 0) {
          val t = create(v)(m / 2)
          Tree.Branch(t, v, t)
        } else {
          val (l, r) = create2(v)((m - 1) / 2)
          Tree.Branch(l, v, r)
        }
      }

    def isBalanced(tree: Tree[A]): Boolean = {
      def count(t: Tree[A]): Int = {
        t match {
          case Tree.E => 0
          case Branch(left, elem, right) =>
            1 + count(left) + count(right)
        }

      }
      tree match {
        case Tree.E => true
        case Tree.Branch(l, v, r) =>
          math.abs(count(l) - count(r) + 1) <= 1
      }

    }
  }

  /** ex 2.1 traverse the list once o(n) complexity This builds up the stack
    * since it is not tail recursive o(n)
    */
  def suffixes[A](l: List[A]): List[List[A]] = l match {
    case Nil    => Nil
    case h :: t => (h :: t) :: suffixes(t)
  }
}
