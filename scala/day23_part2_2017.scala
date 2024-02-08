object Solution extends App {
  def isPrime(n: Int): Boolean = {
    var i = 2
    while (i*i <= n) {
      if (n % i == 0) {
        return false
      }
      i += 1
    }
    true
  }

  val b = 57*100 + 100000
  val c = b + 17000
  var h = 0

  for (x <- b to c by 17) {
    if (!isPrime(x)) {
      h += 1
    }
  }

  println(h)
}