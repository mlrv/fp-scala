import datastructures._

object ListInitObject {
  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

}
