import option._

object SequenceObject {
  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case x :: xs => x.flatMap(y => sequence(xs).map(z => y :: z))
  }
}
