package funbook

import scala.annotation.tailrec

/**
  * Created by Jose on 10/02/2016.
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error("tail of empty list")
    case Cons(_,xs) => xs

  }

  def setHead[A](h: A, l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("setHead of empty list")
      case Cons(_, t) => Cons(h, t)
    }
  }

  def drop[A](n: Int, xss: List[A]): List[A] = n match {
    case 0 => xss
    case n => xss match {
      case Nil => Nil
      case _ => drop(n-1, tail(xss))
    }
  }

  def dropWhile[A](xss: List[A], f: A => Boolean): List[A] = xss match {
    case Nil => Nil
    case Cons(x, xs) => if(f(x)) dropWhile(xs, f)
                        else xss
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  }

  def init[A](l: List[A]) : List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x,init(xs))
    }

  }

  def dropWhileCurried[A](xss: List[A])( f: A => Boolean): List[A] = xss match {
    case Nil => Nil
    case Cons(x, xs) => if(f(x)) dropWhileCurried(  xs)(f)
    else xss
  }


  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }


  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }


  def foldLeft2[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    foldRight(reverse(as), z)((x,y) => f(y,x))
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B= {
    foldLeft(reverse(as), z)((x,y) => f(y,x))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns,1.0)(_ * _)

  def product3(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)( (_,acc) => acc + 1)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)( (acc,h) => acc + 1)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])( (t, h) => Cons(h, t))
  }


  def append2[A](l: List[A], ll: List[A]) = {
    foldRight2(l, ll)((x,y) => Cons(x,y))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight2(l, funbook.Nil: List[A])(append2)
  }

  def add1(l: List[Int]) = {
    foldRight2(l, Nil: List[Int])( (x,y) => Cons(x+1,y))
  }

  def doubTString(l: List[Double]) = {
    foldRight2(l, Nil: List[String])( (x,y) => Cons(x.toString,y))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight2(as, Nil: List[B])( (h,t) => Cons(f(h), t))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean) = {
    foldRight2(as, Nil: List[A])( (h,t) => {
      if(f(h))
        Cons(h,t)
      else t
    })
  }

  def filter_2[A](as: List[A])(f: A => Boolean) = {
    flatMap(as)( a => if(f(a)) List(a) else Nil)
  }

  def addList(as: List[Int], bs: List[Int]): List[Int] = {
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addList(t1,t2))

    }
  }

  def zip[A,B](as: List[A], bs: List[B]): List[(A,B)] = {
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1,h2), zip(t1, t2))
    }
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C) : List[C] = {
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => false
      case (Nil, _) => false
      case (xs, Cons(h1,t1)) => {
        val partial = dropWhileCurried(xs)(a => a != h1)
        partial match {
          case Nil => false
          case 
        }
      }
    }

  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}
