import scala.collection.mutable.ListBuffer

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

  // P10 same solution as built in, only creating def for sake of separate defs for easier testing calls
  def encode[A](list: List[A]): List[(Int, A)] = {
    pack(list)
      .map(l => (l.length, l.head))
  }

  // P11 implemented with built in, simple map over the encoded list of tuples
  def encodeModifiedBuiltIn[A](list: List[A]): List[Any] = {
    encodeBuiltIn(list).map(t => if(t._1 == 1) t._2 else t)
  }

  // P11 implemented same as above with better named lambda variables
  def encodeModified[A](list: List[A]): List[Any] = {
    encode(list).map { case(frequency, element) =>
        if(frequency == 1) element
        else (frequency, element)
    }
  }

  // P12 implemented with built in, utilizing List.fill function
  def decodeBuiltIn[A](list: List[(Int, A)]): List[A] = {
    list.flatMap { case(freq, a) =>
        List.fill(freq)(a)
    }
  }

  // P12 creating a List via flatMap which is populated based on the frequency of the element to be decoded
  def decode[A](list: List[(Int, A)]): List[A] = {
    list.flatMap { case(freq, a) =>
      var as = List.empty[A]
      for (_ <- 0 until freq) {
        as = as :+ a
      }
      as
    }
  }

  // P13 using foldLeft to traverse to look into intermediate tuple List. Then either append number or add new element
  def encodeDirectBuiltIn[A](list: List[A]): List[(Int, A)] = {
    list.foldLeft(List.empty[(Int, A)]) { case(tupleList, element) =>
      if(tupleList.exists(_._2 == element)) {
        val frequency = tupleList.find(_._2 == element).map(_._1).get
        tupleList.filterNot(_._2 == element) :+ (frequency + 1 -> element)  // removing entry and adding updated one
      } else {
        tupleList :+ (1, element)
      }
    }
  }

  // P13 using foreach and populating the tupleList based on whether or not each item is in there already, will
  // append appropriately once tupleList is populated
  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    var tupleList: List[(Int, A)] = List.empty
    list.foreach { a =>
      if(tupleList.isEmpty) tupleList = tupleList :+ (1, a)
      else {
        tupleList = tupleList.map { case(freq, element) =>
          if(element == a) (freq + 1, element)
          else (freq, element)
        }
      }
    }
    tupleList
  }

  // P14 traverse through the list and produce 2 of each iterator via a List, flatten the whole thing at the end
  def duplicate[A](list: List[A]): List[A] = {
    list.flatMap(a => List(a, a))
  }

  // P15 using simple List.fill that creates a sublist that contains specified amount of duplicates
  def duplicateNBuiltIn[A](n: Int, list: List[A]): List[A] = {
    list.flatMap(a => List.fill(n)(a))
  }

  // P15 flatmap and creating intermediate list with a simple loop that populates with simple for loop
  def duplicateN[A](n: Int, list: List[A]): List[A] = {
    list.flatMap { a =>
      var subList: ListBuffer[A] = ListBuffer.empty
      for (_ <- 0 until n) {
        subList = subList :+ a
      }
      subList
    }
  }

  // P16 using zip with index to keep track of each index then flatMap with empty or List containing element based on position
  def dropBuiltIn[A](n: Int, list: List[A]): List[A] = {
    list.zipWithIndex.flatMap { case(a, index) =>
      if((index + 1) % n == 0) List.empty
      else List(a)
    }
  }

  // P16 utilizing recursion based that is looped for each position, only tail goes through recursed function if the head
  // is found to satisfy predicate
  def drop[A](n: Int, list: List[A]): List[A] = {
    def loop(currentPosition: Int, l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case _ :: tail if currentPosition % n == 0 => loop(currentPosition + 1, tail)
        case _ => loop(currentPosition + 1, l)
      }
    }
    loop(1, list)
  }

  // P17 utilizing built in that does exactly what is asked
  def splitBuiltIn[A](splitPoint: Int, list: List[A]): (List[A], List[A]) = {
    list.splitAt(splitPoint)
  }

  // P17 2 sets of traversals through list, first the split point, then from the split point
  def split[A](splitPoint: Int, list: List[A]): (List[A], List[A]) = {
    var firstBuffer: ListBuffer[A] = ListBuffer.empty
    var secondBuffer: ListBuffer[A] = ListBuffer.empty

    for (i <- 0 until splitPoint) {
      firstBuffer = firstBuffer :+ list(i)
    }
    for (j <- splitPoint until list.size) {
      secondBuffer = secondBuffer :+ list(j)
    }

    (firstBuffer.toList, secondBuffer.toList)
  }

  // P18
  def slice[A](i: Int, k: Int, list: List[A]): List[A] = {
    list.zipWithIndex
      .dropWhile { case (_, index) => index <= i && index >= k }
      .map(_._1)
  }
}
