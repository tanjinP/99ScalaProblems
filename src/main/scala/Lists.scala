
/**
  * Contains all problems relative to Lists, P01-P28
  * Implementing both built in Scala solution if possible and algorithm from scratch
  */
object Lists {

  // P1 implemented with Scala built in functions
  def lastBuiltIn[A](list: List[A]): A = {
    if(list.isEmpty) throw new NoSuchElementException
    list.last
  }

  // P1 using pattern matching on List
  def last[A](list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException  // empty List scenario
      case single :: Nil => single                  // single item list scenario
      case _ :: tail => last(tail)                  // recursion through list to eventually get to last element
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
      case Nil => throw new NoSuchElementException    // empty List scenario
      case secondToLast :: _ :: Nil => secondToLast   // returns the second to last element
      case _ :: tail => penUltimate(tail)             // recursion through list to eventually get to penultimate element
    }
  }

}
