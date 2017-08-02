package week2

/**
  * Created by viccardo on 7/02/17.
  */
object Main extends App {
  println("hello")
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  println("x - y - z = " + (x - y - z))
  // it should be 10/7 instead of 70/49
  println("y + y = " + (y + y))
  //in order to fix it we add the method gcd the ones that calculates the commom divisor

  println("x.less(y) = " + (x < y))

  //val strange = new Rational(1,0)
  //println("strange.add(strange) = " + strange.add(strange))

}

class Rational(x: Int, y: Int) {

  def this(x: Int) = this(x, 1)

  require( y!= 0, "denominator must be non zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  val g = gcd(x, y)

  def numer = x / g
  def denom = y / g


  def +(that: Rational) =
    new Rational (numer * that.denom + that.numer*denom,
      denom*that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  // -that is the new form of that.neg
  // in this case we have to use unary_ because it is just one argument
  def - (that: Rational): Rational = this + -that

  def < (that: Rational): Boolean = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  override def toString = numer + "/" + denom

}
