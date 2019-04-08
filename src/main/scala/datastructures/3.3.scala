import datastructures._

object SetHeadObject {
  def setHead[A](xs: List[A], head: A): List[A] = xs match {
    case Nil => Cons(head, Nil)
    case Cons(x, xs) => Cons(head, xs)
  }
}
