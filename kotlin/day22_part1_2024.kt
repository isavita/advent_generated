
import java.io.File
import java.math.BigInteger

fun nextSecret(s: BigInteger): BigInteger {
    var x = s.shiftLeft(6)
    var temp = s.xor(x)
    temp = temp.and(BigInteger("FFFFFF", 16))
    x = temp.shiftRight(5)
    temp = temp.xor(x)
    temp = temp.and(BigInteger("FFFFFF", 16))
    x = temp.shiftLeft(11)
    temp = temp.xor(x)
    return temp.and(BigInteger("FFFFFF", 16))
}

fun main() {
    val buyers = File("input.txt").readLines().filter { it.isNotBlank() }.map { BigInteger(it) }
    var total = BigInteger.ZERO
    for (b in buyers) {
        var s = b
        for (i in 0 until 2000) {
            s = nextSecret(s)
        }
        total += s
    }
    println(total)
}
