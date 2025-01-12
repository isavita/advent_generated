
import java.io.File
import java.math.BigInteger

const val SIZE = 119315717514047L
//const val SIZE = 10L

fun main() {
    val size = BigInteger(SIZE.toString())
    val iterations = BigInteger("101741582076661")
    var offset = BigInteger.ZERO
    var increment = BigInteger.ONE

    File("input.txt").forEachLine { line ->
        when {
            line == "deal into new stack" -> {
                increment = increment.multiply(BigInteger("-1"))
                offset = offset.add(increment)
            }
            line.startsWith("cut") -> {
                val n = line.split(" ")[1].toInt()
                offset = offset.add(BigInteger(n.toString()).multiply(increment))
            }
            line.startsWith("deal with increment") -> {
                val n = line.split(" ").last().toInt()
                val inv = BigInteger(n.toString()).modInverse(size)
                increment = increment.multiply(inv).mod(size)
            }
        }
    }

    val finalIncr = increment.modPow(iterations, size)

    var finalOffs = increment.modPow(iterations, size)
    finalOffs = BigInteger.ONE.subtract(finalOffs).mod(size)
    val invmod = BigInteger.ONE.subtract(increment).modInverse(size)
    finalOffs = finalOffs.multiply(invmod).mod(size)
    finalOffs = finalOffs.multiply(offset).mod(size)

    var answer = BigInteger("2020").multiply(finalIncr).mod(size)
    answer = answer.add(finalOffs).mod(size)

    println(answer)
}
