import scala.compiletime.ops.boolean
enum LazyList[+A]:
  case Empty
  case Cons(head: () => A, tail: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => List.empty[A]
    case Cons(head, tail) => head() :: tail().toList

  def take(n: Int): LazyList[A] = 
    if n == 0 then return Empty.asInstanceOf[LazyList[A]]
    else this match
      case Empty => Empty.asInstanceOf[LazyList[A]]
      case Cons(head, tail) => Cons(head, () => tail().take(n-1))

  def drop(n: Int): LazyList[A] =
    if n == 0 then this
    else this match
      case Empty => Empty
      case Cons(head, tail) => tail().drop(n-1)

  def foldRight = ???

  def headOption: Option[A] = ???

  def takeWhile(pred: A => Boolean): LazyList[A] = this match
    case Empty => Empty
    case Cons(head, tail) =>
      if pred(head()) 
      then Cons(head, () => tail().takeWhile(pred))
      else Empty


  def exist(pred: A => Boolean): Boolean = ???

  def forAll(pred: A => Boolean): Boolean = ???

  def map[B](f: A => B): LazyList[B] = this match
    case Empty => Empty
    case Cons(head, tail) => Cons(() => f(head()), () => tail().map(f))


  def flatMap[B](f: A => LazyList[B]): LazyList[B] = ???

  def filter(pred: A => Boolean): LazyList[A] = this match
    case Empty => Empty
    case Cons(head, tail) =>
      if pred(head())
      then Cons(head, () => tail().filter(pred))
      else tail().filter(pred)


  def append[B >: A](value: => B): LazyList[B] = Cons(() => value, () => this)

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = (this, that) match
    case Cons(thisHead, thisTail), Cons(thatHead, thatTail) => Cons(() => Some(thisHead(), thatHead()), thisTail().zipAll(thatTail()))
    case Cons(thisHead, thisTail), Empty => ???
    case Empty, Cons(thatHead, thatTail) => ???
    case Empty, Empty => Empty

  def startWith(prefix: LazyList[A]): Boolean = ???
  


object LazyList {
  def cons[A](head: => A, tail: => LazyList[A]): LazyList[A] =
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then Empty
    else cons(as.head, apply(as.tail*))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = {
    f(state) match
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def continually[A](a: A): LazyList[A] = unfold(a)(_ => Some((a, a)))

  def from(n: Int): LazyList[Int] = unfold(n)((x: Int) => Some(x, x + 1))

  def fibs: LazyList[Int] = unfold((0, 1))((x1: Int, x2: Int) => 
    Some(x1, (x2, x1 + x2))
  )
}


val xs = LazyList.fibs
xs.take(10).toList
