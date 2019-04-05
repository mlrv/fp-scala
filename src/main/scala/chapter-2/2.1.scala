object FibonacciModule { 
  def fib(n: Int): Int = n match { 
    case 1 => 1 
    case 2 => 1 
    case _ => fib(n - 1) + fib (n - 2) 
  } 
} 
