package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("singletonSet(2) contains 2") {
    new TestSets {
      assert(contains(s2, 2), "Right test, the s2 set contains the integer 2")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("union contains all elements of each set version 2") {
    new TestSets {
      val s = union(s1, s2)
      val r = union(s, s3)
      assert(contains(r, 1), "Union 1")
      assert(contains(r, 2), "Union 2")
      assert(contains(r, 3), "Union 3")
      assert(!contains(r, 98), "Union 98")
      assert(contains(union(r, singletonSet(98)), 98), "Union 98 version 2")
    }
  }

  test("intersects 1") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersec 1")
      assert(!contains(s, 2), "Intersec 2")
      assert(!contains(s, 3), "Intersec 3")
    }
  }

  test("intersects s13") {
    new TestSets {
      val s13 = union(s1, s3)
      val s = intersect(s3, s13)
      assert(!contains(s, 1), "! contains 1")
      assert(!contains(s, 2), "! contains 2")
      assert(contains(s, 3), "Contains 3")
    }
  }

  test("diff s13 diff s23") {
    new TestSets {
      val s13 = union(s1, s3)
      val s23 = union(s2, s3)
      val s = diff(s13, s23 )
      assert(contains(s, 1), "Contains 1")
      assert(!contains(s, 2), "!Contains 1")
      assert(!contains(s, 3), "!Contains 1")
    }
  }

  test("filter 1,2,3") {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      val s = filter(s123, x => x%2 == 0 )
      assert(!contains(s, 1), "Contains 1")
      assert(contains(s, 2), "Contains 1")
      assert(!contains(s, 3), "Contains 1")
    }
  }

  /*
  I think this approach is good then I realize is the wrong approach
  Check the version II the right approach
  VERSION: I
  */
  test("filter Odd numbers of the set {1,2,3,......,50} Version 1") {
    new TestSets {
      def loopUnion (s: Set, n: Int): Set = if (n<=50) loopUnion(union(s, singletonSet(n)),n+1) else s
      val s1to50 = loopUnion(singletonSet(0), 1)
      val sOddTill50 = filter(s1to50, x => x%2 ==1)

      for (i <- 1 to 50 )
        if (i % 2 == 1)
          assert(contains(sOddTill50, i))
        else assert(!contains(sOddTill50, i))
    }
  }

  /*
  VERSION: II
  */
  test("filter Odd numbers of the set {1,2,3,......,50} Version II") {
    new TestSets {
      val sOddTill50 = filter(x => x>=0 && x<=50, x => x%2 ==1)

      for (i <- 1 to 50 )
        if (i % 2 == 1)
          assert(contains(sOddTill50, i))
        else assert(!contains(sOddTill50, i))
    }
  }



  test("forall: Each number on the set is an odd number") {
    def p1 = (x:Int) => x % 2 == 1
    def p2 = (x:Int) => x % 2 == 0
    val sOddTill50 = filter(x => x >= 0 && x <= 50, p1)
    assert(forall(sOddTill50, p1))
  }

  test("forall: Each number on the set is an odd number not an even number") {
    def p1 = (x:Int) => x % 2 == 1
    def p2 = (x:Int) => x % 2 == 0
    val sOddTill50 = filter(x => x >= 0 && x <= 50, p1)
    assert(!forall(sOddTill50, p2))
  }

  test("exist: In the set {1,2,3,......,50} exist the number 37") {
    new TestSets {
      val sOTo50 = filter(x => x>=0 && x<=50, x => x == x)

      def p1 = (x:Int) => x == 37
      def p2 = (x:Int) => x == 50
      def p3 = (x:Int) => x == 51

      for (i <- 1 to 50 )
          assert(contains(sOTo50, i))

      assert(exists(sOTo50, p1))
      assert(exists(sOTo50, p2))
      assert(!exists(sOTo50, p3))

    }
  }


  test("map: In the set {1,2,3,......,50} multiply each number by 100") {
    new TestSets {
      val sOTo50 = filter(x => x>=0 && x<=50, x => x == x)

      def f = (x:Int) => x * 100

      for (i <- 1 to 50 )
        assert(contains(sOTo50, i))


      val sOTo50By100 = map(sOTo50, f)

      for (i <- 1 to 50 )
        assert(contains(sOTo50By100, (i*100)))

    }
  }

}
