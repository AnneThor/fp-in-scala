import scala.annotation.tailrec

// regular fibonacci sequence
def fib(n: Int): Int = {
  if (n == 1) 0
  else if (n == 2) 1
  else fib(n-1) + fib(n-2)
}

fib(6)

object myModule {

  def abs(n: Int): Int =
    if (n < 0) -n else n

  // using tail recursion
  def fibTail(n: Int): Int = {
    @tailrec
    def _helper(n: Int, prev: Int, curr: Int): Int =
    {
      if (n == 0) {
        curr
      }
      else _helper(n - 1, curr, prev + curr)
    }

    if (n == 1) 0
    else if (n == 2) 1
    else _helper(n - 2, 0, 1)
  }

  // tail recursive factorial
  def factorial(n: Int): Int = {
    @tailrec
    def _fact(n: Int, acc: Int): Int =
      if (n == 1) acc
      else _fact(n - 1, acc * n)
    _fact(n, 1)
  }

  private def output(name: String, n: Int, f: Int => Int): String =
    s"The $name of $n is ${f(n)}"

  def main(args: Array[String]): Unit = {
    println(output("absolute value", -10, abs))
    println(output("factorial", 7, factorial))
    println(output("fib number at", 10, fibTail))
  }
}

println(myModule.main(Array()))

// Implement a parametric polymorphic expression to test if an array is sorted
def isSorted[A](arr: Array[A], f: (A, A) => Boolean): Boolean = {
  @tailrec
  def _isSorted(arr: Array[A], f: (A, A) => Boolean, n: Int = 2): Boolean = {
    if (n >= arr.length) true
    else if (f(arr(n), arr(n-1))) _isSorted(arr, f, n + 1)
    else false
  }
  if (arr.length < 2) true
  else _isSorted(arr, f)
}

def numArr: Array[Int] = Array(5, 2, 7, 6)
def sortedArr: Array[Int] = Array(1, 2, 3, 4)
def strArr: Array[String] = Array("apple", "banana", "lime", "lemon")
def strSort: Array[String] = Array("apple", "banana", "lemon", "lime")

isSorted(numArr, (x: Int, y: Int) => x > y)
isSorted(sortedArr, (x: Int, y: Int) => x > y)
isSorted(strArr, (x: String, y: String) => x > y)
isSorted(strSort, (x: String, y: String) => x > y)

// partial function
def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a, b)

// currying
def currying[A,B,C](f: (A,B) => C): A => B => C =
  a => b => f(a, b)

// uncurrying
def uncurrying[A,B,C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

// def compose
def composeEx[A,B,C](f: B => C, g: A => B): A => C =
  a => f(g(a))

// compose is a common exercise, so this function is available in scala
// f andThen g is the same thing as g compose f
val f = (x: Double) => math.Pi / 2 - x
val cos = f andThen math.sin

