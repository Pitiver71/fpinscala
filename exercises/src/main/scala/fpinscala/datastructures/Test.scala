package fpinscala.datastructures

object Test extends App {
  val l = List(1,2,3,4,5,6,7,8,9)
  println(List.drop(l, 4))
  println(List.dropWhile[Int](l, i => i % 2 != 0))
  println(List.init(l))

  val xs = List("a", "b", "c", "d")
  println(List.foldLeft(xs, "")((s1, s2) => s1 + " " + s2))
  println(List.foldRight(xs, "")((s1, s2) => s1 + " " + s2))
}
