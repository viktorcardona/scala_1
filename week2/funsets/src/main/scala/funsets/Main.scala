package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(743), 1))
  println(contains(singletonSet(743), 743))
  println(singletonSet(743).toString())
  println(printSet(singletonSet(743)))
  println(printSet(singletonSet(2300)))
}
