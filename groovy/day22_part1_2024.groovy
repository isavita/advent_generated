
def processSecret(long secret) {
    long temp = secret * 64
    secret = (secret ^ temp) % 16777216
    temp = secret / 32
    secret = (secret ^ temp) % 16777216
    temp = secret * 2048
    secret = (secret ^ temp) % 16777216
    return secret
}

def solve() {
    def initialSecrets = new File('input.txt').readLines().collect { it.toLong() }
    long sum = 0
    initialSecrets.each { initialSecret ->
        long secret = initialSecret
        2000.times {
            secret = processSecret(secret)
        }
        sum += secret
    }
    println sum
}

solve()
