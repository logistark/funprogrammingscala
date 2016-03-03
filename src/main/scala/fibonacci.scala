import scala.annotation.tailrec

/**
  * Created by Jose on 17/01/2016.
  */
object fibonacci {
  def fib(n: Int): Int = {

    def fibAcc(n: Int, prev: Int, act: Int): Int =
      n match {
        case n if n == 0 => prev
        case n =>  fibAcc(n-1, act, prev+act)

    }
    fibAcc(n,0,1)
  }

}
