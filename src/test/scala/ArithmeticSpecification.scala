import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

object ArithmeticSpecification extends Properties("Arithmetic"){

  val primeGen: Gen[Int] = {
    Gen.choose(1, 100000).retryUntil(int =>
      (2 until int).forall(int % _ != 0)
    )
  }

  property("P31: isPrime") = forAll(primeGen){ prime =>
    Arithmetic.isPrime(prime)
  }
}