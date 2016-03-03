import scala.annotation.tailrec

/**
  * Created by Jose on 17/01/2016.
  */
object MyModule {

  def abs(x: Int): Int = {
    if(x < 0) -x
    else x
  }

  def fact(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int ={
      if(n <= 0) acc
      else loop(n-1, n*acc)
    }
    loop(n, 1)
  }

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  def formatFactorial(n: Int) = {
    val msg = "The factorial value of %d is %d."
    msg.format(n, fact(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(acc: Int): Boolean = {
      if (acc >= as.length) false
      else if(acc+1 >= as.length) true
      else if(ordered(as(acc), as(acc+1))) loop(acc+1)
      else false
    }
    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a,b)


  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a: A, b: B) => f(a)(b)
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }

}
