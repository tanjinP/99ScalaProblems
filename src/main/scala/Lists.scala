
/**
  * Contains all problems relative to Lists, P01-P28
  * Implementing both built in Scala solution if possible and algorithm from scratch
  */
object Lists {

  // P1 implemented with Scala built in functions
  def lastBuiltIn[A](list: List[A]): A = {
    if (list.isEmpty) throw new NoSuchElementException
    list.last
  }

  // P1 using pattern matching on List
  def last[A](list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException // empty List scenario
      case single :: Nil => single // single item list scenario
      case _ :: tail => last(tail) // recursion through list to eventually get to last element
    }
  }

  // P2 implemented with Scala built in functions
  def penUltimateBuiltIn[A](list: List[A]): A = {
    if (list.size < 2) throw new IllegalArgumentException("Input list must contain at least 2 elements")
    list.takeRight(2).head
  }

  // P2 using pattern matching on List
  def penUltimate[A](list: List[A]): A = {
    if (list.size < 2) throw new IllegalArgumentException("Input list must contain at least 2 elements")
    list match {
      case Nil => throw new NoSuchElementException // empty List scenario
      case secondToLast :: _ :: Nil => secondToLast // returns the second to last element
      case _ :: tail => penUltimate(tail) // recursion through list to eventually get to penultimate element
    }
  }

  // P3 implemented with Scala built in functions
  def nthBuiltIn[A](index: Int, list: List[A]): A = {
    if (index < 0) throw new IllegalArgumentException
    list(index)
  }

  // P3 using pattern matching on index and List, doing down index until we reach the element of interest
  def nth[A](index: Int, list: List[A]): A = {
    (index, list) match {
      case (0, h :: _) => h
      case (n, _ :: tail) => nth(n - 1, tail)
      case (_, Nil) => throw new IllegalArgumentException
    }
  }

  // P4 implemented with Scala built in functions
  def lengthBuiltIn[A](list: List[A]): Int = {
    list.length
  }

  // P4 using pattern matching and recursion, increment size count as we traverse through the List
  def length[A](list: List[A]): Int = {
    def loop(elements: List[A], size: Int): Int = {
      elements match {
        case Nil => size
        case _ :: tail => loop(tail, size + 1)
      }
    }
    loop(list, 0)
  }

}
