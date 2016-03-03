import scala.annotation.tailrec

/**
  * Created by Jose on 17/01/2016.
  */
object factorial {
  def fact(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int ={
      if(n <= 0) acc
      else loop(n-1, n*acc)
    }
    loop(n, 1)
  }
}
