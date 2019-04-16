import option._

object TraverseObject {
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    SequenceObject.sequence(as.map(f))
}
