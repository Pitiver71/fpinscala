import fpinscala.gettingstarted.MyModule._

def testfib(n: Int) = {
    val ns =for {
        i <- 0 to n
    } yield fib(i)

    print(ns.mkString(" "))
}




