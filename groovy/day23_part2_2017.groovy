
def isPrime(n) {
    for (i = 2; i*i <= n; i++) {
        if (n % i == 0) {
            return false
        }
    }
    return true
}

def inputFile = new File("input.txt")
def lines = inputFile.readLines()

def b = 57*100 + 100000
def c = b + 17000
def h = 0

for (def x = b; x <= c; x += 17) {
    if (!isPrime(x)) {
        h++
    }
}

println(h)
