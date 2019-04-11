package datastructures 

sealed trait List[+A] 
case object Nil extends List[Nothing] 
case class Cons[+A](head: A, tail: List[A]) extends List[A] 


object List { 

def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match { 
  case Nil => z 
  case Cons(h, t) => f(h, foldRight(t, z)(f)) 
} 

@annotation.tailrec 
def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match { 
  case Nil => z 
  case Cons(h, t) => foldLeft(t, f(z, h))(f) 
} 

def sum(ints: List[Int]): Int = ints match { 
  case Nil => 0 
  case Cons(x, xs) => x + sum(xs)  
} 

def product(ds: List[Double]): Double = ds match { 
  case Nil => 1.0 
  case Cons(0.0, _) => 0.0 
  case Cons(x, xs) => x * product(xs) 
} 

def sum2(ns: List[Int]): Int = 
  foldRight(ns, 0)((x, y) => x + y) 

def product2(ds: List[Double]): Double = 
  foldRight(ds, 1.0)((x, y) => x * y) 

def length[A](xs: List[A]): Int = 
  foldRight(xs, 0)((x, y) => y + 1) 

def sum3(ns: List[Int]): Int = 
  foldLeft(ns, 0)(_ + _)  

def product3(ds: List[Double]): Double = 
  foldLeft(ds, 0.0)(_ * _) 

def length2[A](xs: List[A]): Int = 
  foldLeft(xs, 0)((x, y) => x + 1) 

def inverse[A](xs: List[A]): List[A] = 
  foldLeft(xs, List[A]())((acc, h) => Cons(h, acc)) 

def concat[A](a: List[A], b: List[A]): List[A] = 
  foldRight(a, b)((x, y) => Cons(x, y)) 

def flatten[A](xs: List[List[A]]): List[A] = 
  foldRight(xs, List[A]())((x, y) => concat(x, y)) 

def addOneToEach(xs: List[Int]): List[Int] = 
  foldRight(xs, List[Int]())((x, y) => Cons(x + 1, y)) 

def mapDToString(xs: List[Double]): List[String] = 
  foldRight(xs, List[String]())((x, y) => Cons(x.toString, y)) 

def map[A, B](xs: List[A])(f: A => B): List[B] = 
  foldRight(xs, List[B]())((x, y) => Cons(f(x), y)) 

def filter[A](xs: List[A])(p: A => Boolean): List[A] = 
  foldRight(xs, List[A]())((x, y) => p(x) match { 
    case true => Cons(x, y) 
    case false => y 
  }) 

def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = 
  foldRight(xs, List[B]())((x, y) => concat(f(x), y)) 

def filterViaFlatMap[A](xs: List[A])(p: A => Boolean): List[A] = 
  flatMap(xs)(x => p(x) match { 
    case true => List(x) 
    case false => Nil 
  }) 

def addWithZip(fst: List[Int], snd: List[Int]): List[Int] =
  (fst, snd) match {
    case (fst, Nil) => Nil
    case (Nil, snd) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addWithZip(t1, t2))
  }

def apply[A](as: A*): List[A] =  
  if (as.isEmpty) Nil 
  else Cons (as.head, apply(as.tail: _*)) 

} 
