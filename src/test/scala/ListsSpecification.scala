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
    l.nonEmpty ==> (Lists.packBuiltIn(l) exists Lists.pack(l).contains)
  }

  property("P10: encode") = forAll { l: List[Int] =>
    l.nonEmpty ==> (Lists.encodeBuiltIn(l) exists Lists.encode(l).contains)
  }

  property("P11: encodeModified") = forAll { l: List[Int] =>
    l.nonEmpty ==> (Lists.encodeModifiedBuiltIn(l) exists Lists.encodeModified(l).contains)
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
}
