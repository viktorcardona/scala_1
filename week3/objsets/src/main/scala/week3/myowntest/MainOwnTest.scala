package week3.myowntest

/**
  * Created by viccardo on 15/02/17.
  */
object MainOwnTest {

  def main(args: Array[String]): Unit = {
    println("Hello this is MainOwnTest")
    val myList = List(1,2,3,4,5,6,7)
    val n = 7
    println("select the elment "+n+" of the List: ")
    //println(select(n, myList))
  }

  def select(n: Int, list: List[Int]): Int = {
    //if( n < 0 || n > list.size -1) throw new IndexOutOfBoundsException("The element "+n+ " does not exist!")
    //else at(n, list)
    //else list.take(n)
    0
  }

  def at(n: Int, list: List[Int]): Int = {
    if(n == 0) list.head
    else at(n - 1, list.tail)
  }

}
