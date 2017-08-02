package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("sqrt computation by newtons method:")
    val value = 1.0e50
    println("sqrt(" + value + "): " + sqrt(value))

    println("Fixed Point: ")
    val fixed = fixedPoint(x => 1 + x/2) (1)
    println(fixed)
  }

  /**
    * Exercise 1: Pascal’s Triangle
    *
    * The following pattern of numbers is called Pascal’s triangle.
    * 1
    * 1 1
    * 1 2 1
    * 1 3 3 1
    * 1 4 6 4 1
    * ...
    * The numbers at the edge of the triangle are all 1, and each
    * number inside the triangle is the sum of the two numbers above it.
    * Write a function that computes the elements of Pascal’s triangle by means
    * of a recursive process.
    * Do this exercise by implementing the pascal function in Main.scala,
    * which takes a column c and a row r, counting from 0 and returns the
    * number at that spot in the triangle. For example,
    * pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.

    * def pascal(c: Int, r: Int): Int

    */
  def pascal(c: Int, r: Int): Int = {
    if (c > r)
      0
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2: Parentheses Balancing
    *
    * Write a recursive function which verifies the balancing of parentheses in a string, which we represent as a List[Char] not a String. For example, the function should return true for the following strings:
    *
    * (if (zero? x) max (/ 1 x))
    * I told him (that it’s not (yet) done). (But he wasn’t listening)
    * The function should return false for the following strings:
    *
    * :-)
    * ())(
    * The last example shows that it’s not enough to verify that a string contains the same number of opening and closing parentheses.
    *
    * Do this exercise by implementing the balance function in Main.scala. Its signature is as follows:
    * def balance(chars: List[Char]): Boolean
    *
    * There are three methods on List[Char] that are useful for this exercise:
    *
    * chars.isEmpty: Boolean returns whether a list is empty
    * chars.head: Char returns the first element of the list
    * chars.tail: List[Char] returns the list without the first element
    * Hint: you can define an inner function if you need to pass extra parameters to your function.
    *
    * Testing: You can use the toList method to convert from a String to aList[Char]: e.g. "(just an) example".toList.
    */
  def balance(chars: List[Char]): Boolean = {
    balance(chars, 0)
  }

  def balance(chars: List[Char], theBalance: Int): Boolean = {
    if (chars.isEmpty && theBalance == 0)
      true
    else if (chars.isEmpty || theBalance < 0)
      false
    else {
      val head = chars.head
      val openParentheses = head == '('
      val closeParentheses = head == ')'

      if (openParentheses)
        balance(chars.tail, theBalance + 1)
      else if (closeParentheses)
        balance(chars.tail, theBalance - 1)
      else balance(chars.tail, theBalance)
    }
  }

  /**
    * Exercise 3: Counting Change
    *
    * Write a recursive function that counts how many different ways you can make change for an amount,
    * given a list of coin denominations. For example, there are 3 ways to give change for 4
    * if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
    *
    * Do this exercise by implementing the countChange function inMain.scala.
    * This function takes an amount to change, and a list of unique denominations for the coins.
    * Its signature is as follows:
    *
    * def countChange(money: Int, coins: List[Int]): Int
    *
    * Once again, you can make use of functions isEmpty, head and tail on the list of integers coins.
    *
    * Hint: Think of the degenerate cases.
    * How many ways can you give change for 0 CHF(swiss money)?
    * How many ways can you give change for >0 CHF, if you have no coins?
    *
    */

  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0)
      0
    else if (money == 0)
      1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  /*
  * sqrt of a number using newtons method
  * version 0
  *
  */
  def sqrt_v0(x: Double): Double = {
    val estimation = 1
    val quotient = x / estimation
    val mean = (estimation + quotient) / 2
    if (mean * mean == x)
      mean
    else compute_v0(x, mean, mean)
  }

  def compute_v0(x: Double, estimation: Double, lastMean: Double): Double = {
    val quotient = x / estimation
    val mean = (estimation + quotient) / 2
    println(mean)
    if (mean * mean == x || mean == lastMean)
      mean
    else compute_v0(x, mean, mean)
  }

  /*
  * sqrt of a number using newtons method
  * version 1
  *
  */
  def sqrt_v1(x: Double): Double = {
    val estimation = 1
    compute_v1(x, estimation, -1)
  }

  def compute_v1(x: Double, estimation: Double, lastMean: Double): Double = {
    val quotient = x / estimation
    val mean = (estimation + quotient) / 2
    println(mean)
    if (mean * mean == x || mean == lastMean)
      mean
    else compute_v1(x, mean, mean)
  }

  /*
  * sqrt of a number using newtons method
  * version 2
  *
  */
  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else {
        val improveGuess = improve(guess)
        println(improveGuess)
        sqrtIter(improveGuess)
      }
    def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 0.01
    def abs(x: Double) = if (x < 0) -x else x
    def improve(guess: Double) = (guess + x / guess) / 2
    sqrtIter(1)
  }

  //Tail Recursion:
  //If a function calls itself as its last action, the function stack frame can be reused.
  //This is tail recursion.

  //One can require that a function is tail recursive using the annotation:
  //  @tailrec
  //  def method(a: Int, b: Int)
  //If the implementation of the method is not tail recursive, an error would be issued

  //tail recursive is similar to a loop in imperative programming (it good)
  //This factorial method is not tail-recursive:
  //  because the recursive call is sticky with the multiplication operation
  //so it will fail with huge numbers
  //we have to isolate the recursive call in order to make the function tail-recursive
  def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

  //This factorial method is tail recursive
  def factorial2(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }

  /**
    *
    * **********************
    * Higher Order Functions
    * **********************
    *
    * Exercice:
    *
    *
    * Write a tail recursion version of sum:
    *
    * This is the sum of squares:
    *
    * sum(x => x * x, 3, 5) = 50 = 3*3 + 4*4 + 5*5
    * as you saw if f is x*x we got the sum of squares
    *
    */
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  /**
    *
    * **********************
    * Higher Order Functions
    * **********************
    *
    * Exercice:
    *
    * write a function that receives a funtion and then
    *   it is apply for each argument then multiply each result:
    */
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  product(x => x*x) (3, 4)//result=144=(3*3)*(4*4)

  /**
    * define factorial in terms of the product function:
    */
  def factorial_reload(n: Int): Int = product(x => x) (1, n)

  /**
    * write a more general function, which generalizes both sum and product
    * in this case we have to pass 2 parameters:
    *   1) the value when a > b
    *   2) the operation sum or product
   */

  def mapReduce (f: Int => Int, combine: (Int, Int) => Int, zero: Int ) (a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero) (a + 1, b))

  //Now write product in terms of mapReduce:
  def product_withMapReduce(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1) (a, b)

  //use the new product function with map reduce
  product_withMapReduce(x => x*x) (3, 4)//result=144=(3*3)*(4*4)


  /**
    * Fixed Point Algorithm
    * The value x where:
    * f(x) = x
    *
    */
  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) = math.abs( (x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double) (firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  //Now find the value where:
  // f(x) = x
  // where f(x) = 1 + x/2
  // the solution is: 1.999755859375 as expected almost 2
  fixedPoint(x => 1 + x/2) (1)

  //Now we can write square funtion with fixed point:
  def sqrt_with_fixed_point (x: Double) = fixedPoint(y => (y + x / y) / 2 ) (1)

  // Now the new function average is present:
  def averageDamp( f: Double => Double) (x: Double) = ( x + f(x) ) / 2

  //Now rewrite sqrt function in terms of fixed point and the average damp function
  // remember:
  // sqrt (x) = y
  // this is:
  // y*y = x
  // y = x/y
  def sqrt_with_fixed_point_avg (x: Double) = fixedPoint( averageDamp( y => x / y) ) (1)


}