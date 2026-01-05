enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] = this match
    case error@Left(_) => error.asInstanceOf[Either[E, B]]
    case Right(a) => Right(f(a))
  
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case error@Left(_) => error.asInstanceOf[Either[EE, B]]
    case Right(a) => f(a)
  
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
	  case Left(_) => b
    case a@_ => a

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    a <- this
    b <- that
  } yield f(a, b)

object Either {
  def sequence[E,A](as: List[Either[E, A]]): Either[E, List[AA]] = ???

  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???
}
