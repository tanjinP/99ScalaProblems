object Arithmetic {

  // P31
  def isPrime(n: Int): Boolean = {
    !(2 until math.sqrt(n).toInt).exists(n % _ == 0)
  }
}
