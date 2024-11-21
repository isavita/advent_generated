
import java.math.BigInteger

def SIZE = BigInteger.valueOf(119315717514047)
def ITER = BigInteger.valueOf(101741582076661)

def offset = BigInteger.ZERO
def increment = BigInteger.ONE

new File('input.txt').eachLine { line ->
    if (line == 'deal into new stack') {
        increment = increment.multiply(BigInteger.valueOf(-1))
        offset = offset.add(increment)
    } else if (line.startsWith('cut')) {
        def n = Integer.parseInt(line.split()[1])
        offset = offset.add(BigInteger.valueOf(n).multiply(increment))
    } else if (line.startsWith('deal with increment')) {
        def n = Integer.parseInt(line.split()[3])
        increment = increment.multiply(BigInteger.valueOf(n).modInverse(SIZE))
    }
}

def finalIncr = increment.modPow(ITER, SIZE)
def finalOffs = (BigInteger.ONE.subtract(finalIncr)).multiply(BigInteger.ONE.subtract(increment).modInverse(SIZE)).multiply(offset).mod(SIZE)
def answer = (BigInteger.valueOf(2020).multiply(finalIncr).add(finalOffs)).mod(SIZE)

println answer
