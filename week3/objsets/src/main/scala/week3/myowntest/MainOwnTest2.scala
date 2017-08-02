package week3.myowntest

/**
  * Created by viccardo on 15/02/17.
  */
object MainOwnTest2 extends App{

  val list = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil))))

  println("Element: " + select(4, list))

  def select [T] (n: Int, list: List[T]): T =
    if (list.isEmpty) throw new IndexOutOfBoundsException("Wrong element in the list.")
    else if (n == 0) list.head
    else select(n - 1, list.tail)

}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

