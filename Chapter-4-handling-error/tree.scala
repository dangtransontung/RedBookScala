enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def depth: Int = 1

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(i) => Leaf(f(i))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](accum: B, f: (A, B) => B): B = this match
    case Leaf(i) => f(i, accum)
    case Branch(l, r) =>
      val leftFold = l.fold(accum, f)
      r.fold(
        l.fold(accum, f), 
        f
      )

object Tree {
  extension (t: Tree[Int]) def firstPos: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val pos = l.firstPos
      if pos > 0 then pos else r.firstPos  

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l, r) => l.maximum max r.maximum
}
