import either._

object SequenceTraverseObject {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case x :: xs => x.flatMap(y => sequence(xs).map(z => y :: z))
  }

  def traverse[E, A, B](as: List[A])(f:  A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case x :: xs => sequence(xs.map(f))
  }
}
