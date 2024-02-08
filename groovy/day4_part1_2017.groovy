
def file = new File("input.txt")
def passphrases = file.text.trim().split("\n")
def validCount = 0

passphrases.each { passphrase ->
    def words = passphrase.tokenize()
    def wordSet = [:].withDefault { false }

    def valid = true
    words.each { word ->
        if (wordSet[word]) {
            valid = false
            return
        }
        wordSet[word] = true
    }

    if (valid) {
        validCount++
    }
}

println validCount
