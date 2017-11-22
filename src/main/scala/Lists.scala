
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
  def penultimateBuiltIn[A](list: List[A]): A = {
    if (list.size < 2) throw new IllegalArgumentException("Input list must contain at least 2 elements")
    list.takeRight(2).head
  }

  // P2 using pattern matching on List
  def penultimate[A](list: List[A]): A = {
    if (list.size < 2) throw new IllegalArgumentException("Input list must contain at least 2 elements")
    list match {
      case Nil => throw new NoSuchElementException // empty List scenario
      case secondToLast :: _ :: Nil => secondToLast // returns the second to last element
      case _ :: tail => penultimate(tail) // recursion through list to eventually get to penultimate element
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

  // P5 implemented with Scala built in functions
  def reverseBuiltIn[A](list: List[A]): List[A] = {
    list.reverse
  }

  // P5 implemented with pattern marching and recursion
  def reverse[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case h :: tail => reverse(tail) :+ h
    }
  }

  // P6 implemented with Scala built in functions
  def isPalindromeBuiltIn[A](list: List[A]): Boolean = {
    list == list.reverse
  }

  // P6 implemented using answer from P5
  def isPalindrome[A](list: List[A]): Boolean = {
    val reversedList = reverse(list)
    list == reversedList
  }

  // P7 implemented using recursion to flatten out the List (returns List[Any])
  def flatten[A](list: List[Any]): List[A] = {
    list.flatMap {
      case innerList: List[_] => flatten(innerList)
      case standalone: A => List(standalone)
    }
  }

  // P8 implemented with Scala built in functions
  def compressBuiltIn[A](list: List[A]): List[A] = {
    list.distinct
  }

  // P8 implemented using foldLeft
  def compress[A](list: List[A]): List[A] = {
    list.foldLeft(List.empty[A])((compressed, a) =>
      if(compressed.contains(a)) compressed
      else compressed :+ a
    )
  }

  // P9 implemented with Scala built in functions (using groupBy to produce a map causes order to be lost)
  def packBuiltIn[A](list: List[A]): List[List[A]] = {
    list.groupBy(identity)
      .values
      .toList
  }

  // P9 implemented with recursion and partition
  def pack[A](list: List[A]): List[List[A]] = {
    if(list.isEmpty) List(List.empty)
    else {
      val (packed, next) = list.partition(_ == list.head)
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  // P10 implemented with result of existing builtIn and mapping over each inner list to produce tuple
  def encodeBuiltIn[A](list: List[A]): List[(Int, A)] = {
    packBuiltIn(list)
      .map(l => (l.size, l.head))
  }

}
