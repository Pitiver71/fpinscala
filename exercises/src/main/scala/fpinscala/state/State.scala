package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, r) if i == Int.MinValue => (Int.MaxValue, r)
    case (i, r) if i < 0 => (i * -1, r)
    case (i, r) => (i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val t = RNG.nonNegativeInt(rng)
    (t._1.toDouble, t._2)
  }

  def double(): Rand[Double] = map(nonNegativeInt)(_.toDouble)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val int = RNG.nonNegativeInt(rng)
    val double = RNG.double(rng)

    ((int._1, double._1), int._2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val t = intDouble(rng)
    ((t._1._2, t._1._1), t._2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val d1 = RNG.double(rng)
    val d2 = RNG.double(d1._2)
    val d3 = RNG.double(d2._2)

    ((d1._1, d2._1, d3._1), d3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def loop(count: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      count match {
        case 0 => acc
        case _ =>
          val (i, r) = acc._2.nextInt
          loop( count -1, (i :: acc._1, r))
      }
    }

    loop(count, (List.empty[Int], rng))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a,b), r2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
