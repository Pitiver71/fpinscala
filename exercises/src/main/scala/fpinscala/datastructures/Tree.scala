package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size[A](tree: Tree[A]): Int = {
        def count[A](tree: Tree[A], acc: Int): Int = tree match {
            case _: Leaf[A] => acc +1
            case b: Branch[A] => count(b.left, acc) + count(b.right, acc) + 1
        }

        count(tree, 0)
    }

    def max(t: Tree[Int]): Int = {
        def loop(t: Tree[Int], acc: Int): Int = t match {
            case Leaf(i) => if (i > acc) i else acc
            case Branch(l, r) => loop(r, loop(l, acc))
        }

        loop(t, Int.MinValue)
    }

    def depth(t: Tree[Int]): Int = {
        def loop(t: Tree[Int], acc: Int): Int = t match {
            case Leaf(_) => acc + 1
            case Branch(l, r) => loop(l, acc + 1) max loop(r, acc +1)
        }

        loop(t, 0)
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
}