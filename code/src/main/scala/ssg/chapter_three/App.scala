package ssg.chapter_three

/**
 * Created by kirillprasalov on 23.03.16.
 */
object App {

  def main(args: Array[String]): Unit = {
    println("ex3_1: " + ex3_1)

    println()
    println("ex3_2:")
    println(List.tail(List(1, 2, 3, 4, 5)).get)
    println(List.tail(List(1)).get)
    try {
      println(List.tail(Nil).get)
      List.tail(Nil).get
    } catch {
      case e: Exception => println("Nil has no tail!")
    }

    println()
    println("ex3_3:")
    println(List.setHead(239, List(1, 2, 3, 4, 5)).get)
    println(List.setHead(239, List(1)).get)
    try {
      println(List.setHead(239, Nil).get)
      List.tail(Nil).get
    } catch {
      case e: Exception => println("Nil has no head/tail!")
    }

    println()
    println("ex3_4:")
    println(List.drop(3)(List(1, 2, 3, 4, 5)).get)
    println(List.drop(5)(List(1, 2, 3, 4, 5)).get)
    try {
      println(List.drop(239)(List(1, 2, 3, 4, 5)).get)
      List.tail(Nil).get
    } catch {
      case e: Exception => println("Can't drop more than the list has!")
    }
    try {
      println(List.drop(239)(Nil).get)
      List.tail(Nil).get
    } catch {
      case e: Exception => println("Can't drop from Nil!")
    }

    println()
    println("ex3_5:")
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 239))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 0))

    println()
    println("ex3_6:")
    println(List.init(List(1, 2, 3, 4, 5)))
    println(List.init(List(1)))
    try {
      println(List.init(Nil).get)
    } catch {
      case e: Exception => println("Can't remove elements from Nil!")
    }

    println()
    println("ex3_7:")
    println(List.product(List(1.0, 2.0, 0.0, 3.0, 4.0)))

    println()
    println("ex3_8:")
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

    println()
    println("ex3_9:")
    println(List.length(List(1, 2, 3)))
    println(List.length(Nil))

    println()
    println("ex3_11:")
    println(List.lengthViaFoldLeft(List(1, 2, 3)))
    println(List.lengthViaFoldLeft(Nil))
    println(List.sumViaFoldLeft(List(1, 2, 3)))
    println(List.sumViaFoldLeft(Nil))
    println(List.productViaFoldLeft(List(1, 2, 3)))
    println(List.productViaFoldLeft(Nil))

    println()
    println("ex3_12:")
    println(List.reverse(List(1, 2, 3)))
    println(List.reverseViaFoldRight(List(1, 2, 3)))

    println()
    println("ex3_13:")
    println(List.foldLeftViaFoldRight(List(1, 2, 3), 1)(_ * _))
    println(List.foldRightViaFoldLeft(List(1, 2, 3), 1)(_ * _))

    println()
    println("ex3_14:")
    println(List.appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)))

    println()
    println("ex3_15:")
    println(List.flatten(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

    println()
    println("ex3_16:")
    println(List.addOne(List(1, 2, 3)))

    println()
    println("ex3_17:")
    println(List.doubleToString(List(1.5, 2.6, 3.7)))

    println()
    println("ex3_18:")
    println(List.map(List(1, 2, 3), (x: Int) => x * x))

    println()
    println("ex3_19:")
    println(List.filter(List(1, 2, 3), (x: Int) => x != 2))
    println(List.filter(List(1, 2, 3), (x: Int) => x > 239))

    println()
    println("ex3_20:")
    println(List.flatMap(List(1, 2, 3))((x: Int) => List(x, x)))

    println()
    println("ex3_21:")
    println(List.filterViaFlatMap(List(1, 2, 3), (x: Int) => x != 2))
    println(List.filterViaFlatMap(List(1, 2, 3), (x: Int) => x > 239))

    println()
    println("ex3_22:")
    println(List.zipWithSum(List(1, 2, 3), List(4, 5, 6)))

    println()
    println("ex3_24:")
    println(List.hasSub(List(1, 2, 3), List(2, 3)))
    println(List.hasSub(List(1, 2, 3), Nil))
    println(List.hasSub(Nil, List(2, 3)))
    println(List.hasSub(List(1, 2, 3), List(4, 5, 6)))
    println(List.hasSub(Nil, Nil))

    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val leaf3 = Leaf(3)
    val child = Branch(leaf1, leaf2)
    val root = Branch(child, leaf3)

    println()
    println(root)

    println()
    println("ex3_25:")
    println(Tree.size(root))

    println()
    println("ex3_26:")
    println(Tree.max(root))

    println()
    println("ex3_27:")
    println(Tree.depth(root))
    println(Tree.depth(child))
    println(Tree.depth(leaf1))

    println()
    println("ex3_28:")
    println(Tree.map(root)(_ + 1))

    println()
    println("ex3_29:")
    println(Tree.fold(root, 1)(_ * _))
    println(Tree.fold(root, 0)(_ + _))

    println()
    println("foldTailrec:")
    println(Tree.foldTailrec(root, 1)(_ * _))
    println(Tree.foldTailrec(root, 0)(_ + _))
  }

  // should be 3...
  def ex3_1 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
}
