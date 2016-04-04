package fpInScala.chapter03

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  //exercise 2.25
  def countNodes[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + countNodes(l) + countNodes(r)
  }

  //exercise 2.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //exercise 2.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  //exercise 2.28
  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case (Branch(l, r)) => Branch(map(l, f), map(r, f))
  }

  //exercise 2.29
  def fold[A,B](tree : Tree[A], leaf : A => B, branch : (B,B) => B) : B = tree match {
    case Leaf(v) => leaf(v)
    case Branch(l,r)=> branch(fold(l,leaf,branch) , fold(r,leaf,branch))
  }
}
