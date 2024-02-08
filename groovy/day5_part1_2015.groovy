
def niceCount = 0
def vowels = ['a', 'e', 'i', 'o', 'u']
def naughtyStrings = ['ab', 'cd', 'pq', 'xy']

new File('input.txt').eachLine { line ->
    def hasThreeVowels = line.findAll { it in vowels }.size() >= 3
    def hasDoubleLetter = line =~ /(.)\1/
    def hasNaughtyString = naughtyStrings.any { line.contains(it) }

    if (hasThreeVowels && hasDoubleLetter && !hasNaughtyString) {
        niceCount++
    }
}

println niceCount
