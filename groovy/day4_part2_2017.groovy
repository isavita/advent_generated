
def sortString(String w) {
    def s = w.split('').sort().join('')
    return s
}

def validCount = 0
new File("input.txt").eachLine { line ->
    def passphrases = line.trim().split("\n")
    
    passphrases.each { passphrase ->
        def words = passphrase.split(" ")
        def wordSet = [:].withDefault{ false }
        
        def valid = true
        words.each { word ->
            def sortedWord = sortString(word)
            if (wordSet[sortedWord]) {
                valid = false
                return
            }
            wordSet[sortedWord] = true
        }
        
        if (valid) {
            validCount++
        }
    }
}

println validCount
