
def readInputFile(String filename) {
    new File(filename).text.trim().collect { it.toInteger() }
}

def fftPhase(List<Integer> input) {
    def basePattern = [0, 1, 0, -1]
    def output = []

    for (int i = 0; i < input.size(); i++) {
        def pattern = []
        basePattern.each { p -> (i + 1).times { pattern << p } }
        pattern = pattern.drop(1) + pattern.take(1) // Shift pattern left by one

        def sum = 0
        for (int j = 0; j < input.size(); j++) {
            sum += input[j] * pattern[j % pattern.size()]
        }

        output << Math.abs(sum) % 10
    }

    output
}

def fft(List<Integer> input, int phases) {
    (1..phases).inject(input) { acc, _ -> fftPhase(acc) }
}

def main() {
    def input = readInputFile('input.txt')
    def result = fft(input, 100)
    println result.take(8).join('')
}

main()
