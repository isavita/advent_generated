def input = new File("input.txt").readLines().collect { it.toInteger() }

def result = 0
for (int i = 0; i < input.size(); i++) {
    for (int j = i + 1; j < input.size(); j++) {
        if (input[i] + input[j] == 2020) {
            result = input[i] * input[j]
            break
        }
    }
}

println result