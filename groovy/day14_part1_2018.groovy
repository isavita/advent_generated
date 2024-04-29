def input = new File('input.txt').text as int
def scores = [3, 7]
def elf1 = 0
def elf2 = 1

while (scores.size() < input + 10) {
    def sum = scores[elf1] + scores[elf2]
    def digits = sum.toString().split('').collect { it as int }
    scores.addAll(digits)
    elf1 = (elf1 + scores[elf1] + 1) % scores.size()
    elf2 = (elf2 + scores[elf2] + 1) % scores.size()
}

def result = scores.subList(input, input + 10)
println result.join('')