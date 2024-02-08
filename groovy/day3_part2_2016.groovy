
def file = new File("input.txt")
def numbers = file.readLines().collect { it.tokenize().collect { it as int } }

def validTriangles = 0
for (int i = 0; i < numbers[0].size(); i++) {
    for (int j = 0; j < numbers.size(); j += 3) {
        if (j + 2 < numbers.size() && isValidTriangle(numbers[j][i], numbers[j + 1][i], numbers[j + 2][i])) {
            validTriangles++
        }
    }
}

println validTriangles

def isValidTriangle(a, b, c) {
    a + b > c && a + c > b && b + c > a
}
