package bst
import bst.Btree.Branch
import bst.Btree.Leaf
import bst.Btree.Empty
import cats.implicits._
import cats.{Order, Show}
import scala.annotation.tailrec
import queue.Queue
import scala.collection.mutable.ArrayBuffer

sealed trait Btree[+A] {

  // TODO not stack safe - implies dfs traversal bc of branch function argument evaluation order.
  def fold[B](empty: B)(leaf: A => B)(branch: (B, B, A) => B): B = this match {
    case Empty   => empty
    case Leaf(v) => leaf(v)
    case Branch(l, r, v) =>
      branch
        .curried(l.fold(empty)(leaf)(branch))(r.fold(empty)(leaf)(branch))(v)
  }

  // left - root - right
  def inOrderTraveral: List[A] =
    fold(List.empty[A])(List(_)) { case (left, right, v) =>
      left ++ List(v) ++ right
    }

  // root - left - right
  def preOrderTraversal: List[A] =
    fold(List.empty[A])(List(_)) { case (left, right, v) =>
      v :: left ++ right
    }

  // left - right - root
  def postOrderTraversal: List[A] =
    fold(List.empty[A])(List(_)) { case (left, right, v) =>
      left ++ right ++ List(v)
    }

  def breadthFirstTraversal: List[A] = {
    import scala.collection.mutable.{Queue, ArrayBuffer}

    val q = Queue(this)
    val arr: ArrayBuffer[A] = ArrayBuffer()

    while (!q.isEmpty) {
      q.dequeue match {
        case Branch(left, right, value) =>
          arr.addOne(value)
          q.enqueue(left, right)
        case leaf @ Leaf(value) =>
          arr.addOne(value)
        case _ => ()
      }

    }
    arr.toList
  }

  def mapByLevel[B](f: (Int, List[A]) => B): List[B] = {
    val updateLevel: (Int, A, Map[Int, List[A]]) => Map[Int, List[A]] =
      (level, value, m) => {
        m.updatedWith(level) {
          case Some(l) => Some(value :: l)
          case None    => Some(List(value))
        }
      }
    def go(
        tree: Btree[A],
        curLevel: Int,
        levels: Map[Int, List[A]]
    ): Map[Int, List[A]] = tree match {
      case b @ Branch(left, right, value) =>
        val nextM = updateLevel(curLevel, value, levels)
        nextM |+| go(left, curLevel + 1, Map.empty) |+| go(
          right,
          curLevel + 1,
          Map.empty
        )
      case Leaf(value) => updateLevel(curLevel, value, levels)
      case Empty       => levels
    }

    go(this, 0, Map.empty).toList.map(f.tupled)
  }

  def mapByLevelV2[B](f: (Int, List[A]) => B): List[B] = this match {
    case Branch(left, right, value) => ???
    case Empty                      => ???
    case Leaf(value)                => ???
  }

  def searchBST[B >: A: Order](value: B): Option[Btree[A]] =
    this match {
      case br @ Branch(left, right, v) =>
        if (value == v) Some(br)
        else if (value < v)
          left.searchBST(value)
        else
          right.searchBST(value)
      case leaf @ Leaf(v) => (value == v).guard[Option].as(leaf)
      case Empty          => None
    }

  // Inserts according to rules of BST
  def insert[B >: A](value: B)(implicit ord: Order[B]): Btree[B] = this match {
    case Branch(left, right, v) =>
      if (value > v)
        Branch(left, right.insert(value), v)
      else
        Branch(left.insert(value), right, v)
    case Leaf(v) =>
      if (value < v)
        Branch(Leaf(value), Empty, v)
      else
        Branch(Empty, Leaf(value), v)
    case Empty => Leaf(value)
  }

  def height: Int = this match {
    case Branch(left, right, v) =>
      List(left.height, right.height).max + 1
    case Leaf(v) => 1
    case Empty   => 0
  }

}

object Btree {
  case class Branch[A](left: Btree[A], right: Btree[A], value: A)
      extends Btree[A]
  case class Leaf[A](value: A) extends Btree[A]
  case object Empty extends Btree[Nothing]

  implicit def forShow[A: Show]: Show[Btree[A]] = new Show[Btree[A]] {

    override def show(t: Btree[A]): String = ???

  }

  def empty[A]: Btree[A] = Empty

  // def bstFromLL[A](v: Vector[A]): Btree[A] = {
  //   if (v.isEmpty) Empty
  //   else if (v.length > 1) {
  //     var childs = 0
  //     def loop(l: List[A]): Btree[A] = l match {
  //       case x :: y =>
  //         Branch()
  //       case Nil => ???
  //     }
  //     loop(v.toList)
  //   } else Leaf(v.head)

  // }

}

object Main extends App {
  val tree0 = List(1, 2, 3, 4, 5).foldLeft(Btree.empty[Int]) { case (tree, n) =>
    tree.insert(n)
  }

  val tree1 = Branch(
    Branch(Leaf(1), Empty, 2),
    Branch(Leaf(3), Leaf(6), 5),
    0
  )

  val tree1Plus7 = Branch(
    Branch(Leaf(1), Empty, 2),
    Branch(Leaf(3), Branch(Empty, Leaf(7), 6), 5),
    0
  )

  val nonBst = Branch(
    Branch(Leaf(4), Leaf(5), 2),
    Leaf(3),
    1
  )

  val treeSingle = Leaf(1)

  nonBst.mapByLevel((i, l) => println(s"Level $i sum is ${l.sum}"))
  nonBst.mapByLevel((i, l) =>
    println(s"Nodes at level $i are ${l.mkString(", ")}")
  )

  assert {
    tree1.searchBST(3) == Some(Leaf(3))
  }

  assert {
    tree1.height == 3
  }

  assert {
    tree1.insert(7) == tree1Plus7
  }

  assert {
    tree0.inOrderTraveral == (1 to 5).toList
  }

  println(nonBst.breadthFirstTraversal)
  assert {
    nonBst.inOrderTraveral == List(4, 2, 5, 1, 3)
    nonBst.preOrderTraversal == List(1, 2, 4, 5, 3)
    nonBst.postOrderTraversal == List(4, 5, 2, 3, 1)
    nonBst.breadthFirstTraversal == List(1, 2, 3, 4, 5)
  }

}
