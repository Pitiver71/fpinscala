package fpinscala.laziness

object Test extends App {

  val s = Stream(1, 2, 3, 4, 5, 6)
  println(s.toList())
  println(s.take(3).toList())
  println(s.drop(4).toList())
  println(s.takeWhile(_ < 4).toList())
  println(s.takeWhile2(_ < 4).toList())
  println(s.forAll(_ < 10))
  println(s.forAll(_ > 10))
  println(s.map(_ + 10).toList())
  println(s.filter(_ % 2 == 0).toList())
  println(s.append(Stream(7, 8, 9)).toList())
  println(s.flapMap(i => Stream(i to i)).toList())
  println(Stream.constant(2).take(10).toList())
  println(Stream.from(45).take(10).toList())
  println(Stream.fib().take(10).toList())
}
