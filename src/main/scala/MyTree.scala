object MyTree extends App {
  sealed trait Tree[T]
  final case class Branch[T](v: T, l: Tree[T], r: Tree[T]) extends Tree[T]
  final case class Leaf[T]() extends Tree[T]

  def addId[T](t:Tree[T]) : Tree[(T,Int)] = addId(t, 1)._1

  private def addId[T](t: Tree[T], i:Int) : (Tree[(T,Int)], Int) = t match {
    case Leaf() => (Leaf(), i)
    case Branch(v, l, r) => {
      val newLeft = addId(l, i)
      val newRight = addId(r, newLeft._2)
      (Branch((v, newRight._2), newLeft._1, newRight._1), newRight._2 + 1)
    }
  }

  // Implementation where the function does not return the index but it is
  // deduced from the tree in function 'getId()'
  def addId2[T](t: Tree[T]) : Tree[(T,Int)] = addId2(t, 0)

  def getId[T](t: Tree[(T,Int)], i:Int) = t match {
    case Leaf() => i
    case Branch(v, _, _) => v._2
  }

  private def addId2[T](t: Tree[T], i: Int) : Tree[(T,Int)] = t match {
    case Leaf() => Leaf()
    case Branch(v, l, r) => {
      val newLeft = addId2(l, i)
      val leftIndex = getId(newLeft, i)
      val newRight = addId2(r, leftIndex)
      val rightIndex = getId(newRight, leftIndex)
      Branch((v,rightIndex + 1), newLeft, newRight)
    }
  }
}