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

  test("test times") {
    new TestTrees {
      assert(times(List('a', 'b', 'd', 'a')).toSet === List(('a',2), ('b', 1), ('d', 1)).toSet)
    }
  }

  test("test makeOrderedLeaList") {
    new TestTrees {
      assert(makeOrderedLeafList(List(('a',2), ('b', 1), ('c', 1))) === List(Leaf('b',1), Leaf('c', 1), Leaf('a', 2)))
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

  test("combine of some leaf list 2") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 2), Leaf('x', 3))
    assert(combine(leaflist) === List(Leaf('x',3), Fork(Leaf('e',2),Leaf('t',2),List('e', 't'),4)))
  }

  test("combine of some leaf list4") {
    val ll = List(Leaf('a', 2), Leaf('b', 2), Leaf('c', 3), Leaf('d', 5))
    assert(combine(ll) === List(Leaf('c', 3), Fork(Leaf('a',2),Leaf('b',2),List('a', 'b'),4), Leaf('d', 5)))
  }

  test("test secret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a very short text should be identity2") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ba".toList)) === "ba".toList)
    }
  }

  test("decode and encode a very short text should be identity3") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abbab".toList)) === "abbab".toList)
    }
  }

  test("decode and quickencode a very short text should be identity3") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("abbab".toList)) === "abbab".toList)
    }
  }
}
