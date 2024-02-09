import java.io.File

fun isPrime(n: Int): Boolean {
    var i = 2
    while (i * i <= n) {
        if (n % i == 0) {
            return false
        }
        i++
    }
    return true
}

fun main(args: Array<String>) {
    val inputFile = File("input.txt")
    val lines = inputFile.readLines()

    val b = 57 * 100 + 100000
    val c = b + 17000
    var h = 0

    for (x in b..c step 17) {
        if (!isPrime(x)) {
            h++
        }
    }

    println(h)
}