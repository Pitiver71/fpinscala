package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList() : List[A] = {
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case Empty => acc
    }

    loop(this, List.empty).reverse
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    def loop(s: Stream[A], i: Int): Stream[A] = s match {
      case Cons(h, t) if i > 1 => cons(h(), loop(t(), i - 1))
      case Cons(h, _) if i == 1 => cons(h(), Empty)
      case _ => Empty
    }

    loop(this, n)
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], i: Int): Stream[A] = s match {
      case Cons(h, t) if i > 0 => loop(t(), i - 1)
      case _ => s
    }

    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Empty => true
    case _ => false
  }

  def map[B](f: A => B) : Stream[B] = this.foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean) : Stream[A] = this.foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](s: Stream[B]): Stream[B] = this.foldRight(s)((h, t) => cons(h, t))

  def flapMap[B](f:A => Stream[B]) : Stream[B] = this.foldRight(empty[B])((h,t) => f(h).append(t))

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fib(): Stream[Int] = {
    def loop(curr: Int, next: Int): Stream[Int] = {
      cons(curr, loop(next, curr + next))
    }

    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

}