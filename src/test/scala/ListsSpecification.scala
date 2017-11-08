import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

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
}
