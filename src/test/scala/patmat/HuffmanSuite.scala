package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman.{Fork, _}

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

  test("times test"){
    val l = List('a','b','a','c')
    val expected = List(('a',2),('b',1),('c',1))
    assert(expected === times(l).sortWith((c1,c2) => c2._2 < c1._2))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val freqs = List(('a',8),('b', 3), ('c', 1),('d', 1),('e', 1),('f', 1),('g', 1),('h', 1))
    val orderedLeafList  = makeOrderedLeafList(freqs)
    assert(combine(orderedLeafList) === List(Leaf('e',1), Leaf('f',1), Leaf('g',1), Leaf('h',1), Fork(Leaf('c',1),Leaf('d',1),List('c', 'd'),2), Leaf('b',3), Leaf('a',8)))
  }

  test("until fuction"){
    val freqs = List(('a',8),('b', 3), ('c', 1),('d', 1),('e', 1),('f', 1),('g', 1),('h', 1))
    val orderedLeafList  = makeOrderedLeafList(freqs)
    assert(until(singleton(_),combine(_))(orderedLeafList) === Fork(Leaf('a',8), Fork(Fork(Fork(Leaf('g',1),Leaf('h',1),List('g','h'),2),Fork(Leaf('e',1),Leaf('f',1),List('e','f'),2),List('e','f','g','h'),4),Fork(Fork(Leaf('c',1),Leaf('d',1),List('c','d'),2),Leaf('b',3),List('b','c','d'),5),List('b','c','d','e','f','g','h'),9),List('a','b','c','d','e','f','g','h'),17))
  }

  test("create code tree function"){
    val chars = List('e','t','t','x','x','x','x')
    assert(createCodeTree(chars) === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e','t','x'),7))
  }


  test("decoding list of bits"){
    val codeTree = Fork(Leaf('a',8), Fork(Fork(Fork(Leaf('g',1),Leaf('h',1),List('g','h'),2),Fork(Leaf('e',1),Leaf('f',1),List('e','f'),2),List('e','f','g','h'),4),Fork(Fork(Leaf('c',1),Leaf('d',1),List('c','d'),2),Leaf('b',3),List('b','c','d'),5),List('b','c','d','e','f','g','h'),9),List('a','b','c','d','e','f','g','h'),17)
    val bits = List(1,0,0,0,0)
    val expected = List('g','a')
    val result = decode(codeTree,bits)
    assert( result=== expected)
  }

  test ("encoding test"){
    new TestTrees {
      val charsToEncode = List('a','b')
      val codedChars = encode(t1)(charsToEncode)
      assert(codedChars === List(0, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("merging code table "){
    val a = List(('a',List(1)),('b',List(1)))
    val b = List(('a',List(0)),('b',List(1)),('c',List(0)))
    val result = mergeCodeTables(a,b)
    assert(result == List(('a',List(1,0)),('b',List(1,1)),('c',List(0))))
  }

}
