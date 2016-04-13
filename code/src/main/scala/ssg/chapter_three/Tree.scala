package ssg.chapter_three

import scala.annotation.tailrec

/**
 * Created by kirillprasalov on 04.04.16.
 */
sealed trait Tree[+A] {
  final override def toString: String = this match {
    case Leaf(a) => a.toString
    case Branch(left, right) => "Tree(" + left.toString + ", " + right.toString + ")"
  }
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(left, right) => max(left) max max(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 0
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
    case Leaf(a) => f(a, z)
    case Branch(left, right) => fold(right, (fold(left, z)(f)))(f)
  }

  //=================================================================

  def foldTailrec[A, B](t: Tree[A], z: B)(f: (A, B) => B) = {
    @tailrec def traverse(l: scala.List[Tree[A]], res: B): B = l match {
      case Leaf(a) :: tail => traverse(tail, f(a, res))
      case Branch(left, right) :: tail => traverse(left :: right :: tail, res)
      case scala.Nil => res
    }
    traverse(scala.List(t), z)
  }
}