package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
    * // = ???
   */
    def singletonSet(elem: Int): Set = set => set == elem
    // my implementation:
    // Set(elem)
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
    *
    * // : Set = ???
   */
    def union(s: Set, t: Set): Set = x => (contains(s,x) || contains(t,x))
  //version 0:
  //x => (s(x) || t(x))
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = x => contains(s,x) && contains(t,x)
  /*
  version 0:
   x => s(x) && t(x)
  */
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = x => contains(s,x) && !contains(t,x)
  /*
  x => s(x) && !t(x)
  */
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)
  //version 0:
  //x => s(x) && p(x)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, p) && !forall(s, x => !p(x))
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
    *
    * def map(s: Set, f: Int => Int): Set = ???
   */
    def map(s: Set, f: Int => Int): Set = x => exists(s, y => s(f(y)) )

  /*
  version 0 (my version):
  {
      def loop(x: Int, s2: Set): Set =
        if(x > bound) s2
        else if (contains(s, x) && s2 == Nil) loop(x  + 1, singletonSet(f(x)))
        else if (contains(s, x) && s2 != Nil) loop(x  + 1, union(s2, singletonSet(f(x))))
        else loop(x  + 1, s2)
    loop(-bound, Nil)
  }

  version 1 (another one's version):
  x => exists(s, y => s(f(y)) )
  */
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
