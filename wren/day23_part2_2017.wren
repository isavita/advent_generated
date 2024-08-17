import "io" for File

var isPrime = Fn.new { |n|
  for (i in 2..n / 2) {
    if (n % i == 0) return false
  }
  return true
}

var main = Fn.new {
  var input = File.read("input.txt")
  var b = 57 * 100 + 100000
  var c = b + 17000
  var h = 0

  var x = b
  while (x <= c + 1) {
    if (!isPrime.call(x)) h = h + 1
    x = x + 17
  }

  System.print(h)
}

main.call()