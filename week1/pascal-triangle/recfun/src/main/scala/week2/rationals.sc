object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x.numer
  x.denom
  println("x.numer: "+x.numer)
  println("x.denom: "+x.denom)

  println("x - y - z: "+x.sub(y).sub(z))
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational (numer * that.denom + that.numer*denom,
                  denom*that.denom)

  def neg: Rational = new Rational(-numer, denom)

  def sub_v0(that: Rational) =
    new Rational (numer * that.denom - that.numer*denom,
      denom*that.denom)

  //even better
  def sub(that: Rational): Rational = add(that.neg)


  override def toString = numer + "/" + denom

}
