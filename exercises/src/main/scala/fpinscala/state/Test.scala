package fpinscala.state

import RNG.Simple

object Test extends App {

  val rng = Simple(Int.MaxValue)
  println("nonNegativeInt -> " + RNG.nonNegativeInt(rng)._1)
  println("double -> " + RNG.double(rng)._1)
  println("ints -> " + RNG.ints(6)(rng)._1)
  println(RNG.map(RNG.int)(_ * 2))

}
