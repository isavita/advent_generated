def input = new File("input.txt").text.trim()

def getNextSequence(String input) {
    def result = ""
    def count = 1
    def current = input[0]

    for (int i = 1; i < input.size(); i++) {
        if (input[i] == current) {
            count++
        } else {
            result += count + current
            current = input[i]
            count = 1
        }
    }

    result += count + current

    return result
}

for (int i = 0; i < 40; i++) {
    input = getNextSequence(input)
}

println input.length()