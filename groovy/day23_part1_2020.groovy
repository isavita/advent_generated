
def file = new File("input.txt")
def input = file.text.trim()

def cups = new int[input.length() + 1]
def currentCup = 0

(0..<input.length()).each { i ->
    def cup = Integer.parseInt(input[i])
    if (i == 0) {
        currentCup = cup
    }
    if (i < input.length() - 1) {
        def nextCup = Integer.parseInt(input[i + 1])
        cups[cup] = nextCup
    }
}

def firstCup = Integer.parseInt(input[0])
def lastCup = Integer.parseInt(input[input.length() - 1])
cups[lastCup] = firstCup

(0..99).each { _ ->
    def pickup1 = cups[currentCup]
    def pickup2 = cups[pickup1]
    def pickup3 = cups[pickup2]

    cups[currentCup] = cups[pickup3]

    def destinationCup = currentCup - 1
    if (destinationCup < 1) {
        destinationCup = input.length()
    }
    while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
        destinationCup--
        if (destinationCup < 1) {
            destinationCup = input.length()
        }
    }

    cups[pickup3] = cups[destinationCup]
    cups[destinationCup] = pickup1

    currentCup = cups[currentCup]
}

def cup = cups[1]
while (cup != 1) {
    print(cup)
    cup = cups[cup]
    if (cup == 1) {
        break
    }
}
println()
