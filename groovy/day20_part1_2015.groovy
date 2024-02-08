
def input = new File("input.txt").text.toInteger()

def sumOfFactors = { n ->
    def sum = 0
    (1..Math.sqrt(n).toInteger()).each { i ->
        if (n % i == 0) {
            sum += i
            if (n / i != i) {
                sum += n / i
            }
        }
    }
    return sum
}

def house = 1
while (sumOfFactors(house) * 10 < input) {
    house++
}

println house
