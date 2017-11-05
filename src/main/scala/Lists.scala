
/**
  * Contains all problems relative to Lists, P01-P28
  * Implementing both built in Scala solution if possible and algorithm from scratch
  */
object Lists {

  def lastBuiltIn[A](list: List[A]): A = {
    list.last
  }

  def last[A](list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException  // empty List scenario
      case single :: Nil => single
      case _ :: tail => last(tail)
    }
  }

}
