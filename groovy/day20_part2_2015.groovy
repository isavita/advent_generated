
def input = new File("input.txt").text.toInteger()
def limit = input / 10

def houses = new int[limit]

for (int i = 1; i < limit; i++) {
    for (int j = i; j < limit; j += i) {
        houses[j] += i * 10
    }
}

def part1 = houses.findIndexOf { it >= input }

houses = new int[limit]

for (int i = 1; i < limit; i++) {
    int count = 0
    for (int j = i; j < limit && count < 50; j += i) {
        houses[j] += i * 11
        count++
    }
}

def part2 = houses.findIndexOf { it >= input }

println part1
println part2
