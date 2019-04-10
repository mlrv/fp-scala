import datastructures._

object ListDropWhileObject {
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(h, t) => if (f(h) == false) xs
                       else dropWhile(t, f)
  }
  def dropWhile2[A](xs: List[A])(f: A => Boolean): List[A] = xs match { 
    case Cons(h, t) if f(h) => dropWhile2(t)(f) 
    case Nil => xs 
  } 
}
