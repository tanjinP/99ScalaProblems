import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAll}

object ListsSpecification extends Properties("Lists") {

  property("P1: last") = forAll { l: List[Int] =>
    l.nonEmpty ==> (l.last == Lists.last(l))
  }

  property("P2: penultimate") = forAll { l: List[Int] =>
    (l.size > 2) ==> (l.takeRight(2).head == Lists.penultimate(l))
  }

  property("P3: nth") = forAll { (i: Int, l: List[Int]) =>
    (i < l.length && i >= 0 && l.nonEmpty) ==> (l(i) == Lists.nth(i, l))
  }

  property("P4: length") = forAll { l: List[Int] =>
    l.size == Lists.length(l)
  }

  property("P5: reverse") = forAll { l: List[Int] =>
    l.reverse == Lists.reverse(l)
  }

  property("P6: isPalindrome") = forAll { l: List[Char] =>
    val palindromeFromBuiltIn = l == l.reverse
    palindromeFromBuiltIn == Lists.isPalindrome(l)
  }

  // TODO test for P7

  property("P8: compress") = forAll { l: List[Int] =>
    l.distinct == Lists.compress(l)
  }

  property("P9: pack") = forAll { l: List[Int] =>
    l.nonEmpty ==> (Lists.packBuiltIn(l) forall Lists.pack(l).contains)
  }

  property("P10: encode") = forAll { l: List[Int] =>
    l.nonEmpty ==> (Lists.encodeBuiltIn(l) forall Lists.encode(l).contains)
  }

  property("P11: encodeModified") = forAll { l: List[Int] =>
    l.nonEmpty ==> (Lists.encodeModifiedBuiltIn(l) forall Lists.encodeModified(l).contains)
  }

  // used for P12
  val intCharList: Gen[List[(Int, Char)]] = Gen.containerOf[List, (Int, Char)]{
    for {
      int <- Gen.choose(1, 100)
      char <- Gen.alphaChar
    } yield (int, char)
  }

  property("P12: decode") = forAll(intCharList) { l: List[(Int, Char)] =>
    l.nonEmpty ==> (Lists.decodeBuiltIn(l) == Lists.decode(l))
  }

  property("P13: encodeDirect") = forAll { l: List[Char] => // TODO need to pass this test
    l.nonEmpty ==> (Lists.encodeDirectBuiltIn(l) forall Lists.encodeDirect(l).contains)
  }

  property("P14: duplicate") = forAll { l: List[Char] =>
    l.nonEmpty ==> (Lists.duplicate(l).size == 2 * l.size)
  }

  property("P15: duplicateN") = forAll { (n: Int, l: List[String]) =>
    (n > 0 && l.nonEmpty) ==> (Lists.duplicateNBuiltIn(n, l) == Lists.duplicateN(n, l))
  }

  property("P16: drop") = forAll { (n: Int, l: List[Int]) =>
    (n > 0 && n <= l.length && l.nonEmpty) ==> (Lists.dropBuiltIn(n, l) forall Lists.drop(n, l).contains)
  }

  property("P17: split") = forAll { (splitPoint: Int, l: List[Char]) =>
    (splitPoint > 0 && splitPoint <= l.size && l.nonEmpty) ==> (Lists.splitBuiltIn(splitPoint, l) == Lists.split(splitPoint, l))
  }
}
