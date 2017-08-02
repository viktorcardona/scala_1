package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("encode a very short text") {
    new TestTrees {
      //tree:
      //val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
      //
      //             List('a','b') 5
      // Leaf('a',2)            Leaf('b',3)
      assert(encode(t1)("ab".toList) === List(0,1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }


  /**
    * New Methods:
    */


  test("Frequency 0!") {
    assert(times(List()) == List())
  }

  test("Frequency 1!") {
    assert(times(List('a')) == List(('a', 1)))
  }

  test("Frequency 2!") {
    assert(times(List('a', 'a')) == List(('a', 2)))
  }

  test("Frequency 3!") {
    assert(times(List('a', 'a', 'a')) == List(('a', 3)))
  }

  test("Frequency 4!") {
    assert(times(List('a', 'a', 'a', 'a')) == List(('a', 4)))
  }

  test("Frequency 5!") {
    assert(times(List('a', 'b', 'a')) == List(('a', 2), ('b', 1)))
  }

  test("Frequency 6!") {
    assert(times(List('a', 'b', 'a', 'a')) == List(('a', 3), ('b', 1)))
  }

  test("Frequency 7!") {
    assert(times(List('a', 'b', 'a', 'a', 'a', 'a', 'a', 'a')) == List(('a', 7), ('b', 1)))
  }


  test("Frequency 8!") {
    assert(times(List('a', 'b', 'a', 'x')) == List(('a', 2), ('x', 1), ('b', 1)))
  }

  test("Frequency 9!") {
    assert(times(List('a', 'b', 'a', 'a', 'x', 'a', 'a', 'a')) == List(('a', 6), ('x', 1), ('b', 1)))
  }

  test("Frequency 10!") {
    assert(times(List('a', 'b', 'c', 'd', 'e', 'f')) == List(('a', 1), ('f', 1), ('b', 1), ('e', 1), ('c', 1), ('d', 1)))
  }

  test("Frequency 11!") {
    assert(times(List('a', 'b', 'c')) == List(('a', 1), ('c', 1), ('b', 1)))
  }

  test("Frequency 12!") {
    assert(times(List('a', 'b')) == List(('a', 1), ('b', 1)))
  }

  test("Frequency 13!") {
    assert(times(List('a', 'b', 'x', 'x', 'x', 'x', 'x', 'x', 'b', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'z'))
      == List(('a', 1), ('z', 1), ('b', 2), ('x', 17)))
  }

  test("makeOrderedLeafList '2nd'") {
    assert(makeOrderedLeafList(List(('t', 77), ('e', 88), ('x', 99))) === List(Leaf('t',77), Leaf('e',88), Leaf('x',99)))
  }

  test("makeOrderedLeafList '3rd'") {
    assert(makeOrderedLeafList(List(('t', 44), ('e', 33), ('x', 22))) === List(Leaf('x',22), Leaf('e',33), Leaf('t',44)))
  }

  test("makeOrderedLeafList '4th'") {
    assert(makeOrderedLeafList(List(('a', 6), ('b', 5), ('c', 4), ('d', 3), ('e', 2), ('f', 1), ('g', 0), ('h', -1))) === List(Leaf('h',-1), Leaf('g',0), Leaf('f',1), Leaf('e',2), Leaf('d',3), Leaf('c',4), Leaf('b',5), Leaf('a',6)))
  }

  test("makeOrderedLeafList '5th'") {
    assert(makeOrderedLeafList(List(('a', 6), ('b', 5), ('c', 4), ('d', 3), ('z', 10), ('e', 2), ('f', 1), ('g', 0), ('h', -1))) === List(Leaf('h',-1), Leaf('g',0), Leaf('f',1), Leaf('e',2), Leaf('d',3), Leaf('c',4), Leaf('b',5), Leaf('a',6), Leaf('z', 10)))
  }

  test("Decoding the secret using the Huffman Coding tree for french language") {
    assert(decode(frenchCode, secret) == "huffmanestcool".toList)
  }

  test("Quick::Encoding then normal Decoding") {
    assert(decode(frenchCode, quickEncode(frenchCode)("huffmanestcool".toList)) == "huffmanestcool".toList)
  }

}
