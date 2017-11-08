import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

object ListsSpecification extends Properties("Lists") {

  property("last") = forAll { l: List[Int] =>
    l.nonEmpty ==> (l.last == Lists.last(l))
  }
}
