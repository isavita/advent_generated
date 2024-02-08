
def file = new File("input.txt")
def input = file.text.trim()

def repeatedInput = repeatInput(input, 10000)

def offset = Integer.parseInt(input[0..6])

for (phase in 1..100) {
    def sum = 0
    for (int i = repeatedInput.size() - 1; i >= offset; i--) {
        sum += repeatedInput[i]
        repeatedInput[i] = sum % 10
    }
}

def message = repeatedInput[offset..(offset + 7)].join()
println message

def repeatInput(input, times) {
    def digits = []
    (0..<times).each { t ->
        input.each { r ->
            def digit = Integer.parseInt(r)
            digits.addAll([digit])
        }
    }
    return digits
}
