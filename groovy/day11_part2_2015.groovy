def inputFile = new File('input.txt')
def inputString = inputFile.text.trim()

def isValidPassword(String password) {
    def hasStraight = false
    def hasPair = false
    def pairs = []
    def lastChar = ''
    def charArray = password.toCharArray()

    for (int i = 0; i < charArray.size(); i++) {
        char c = charArray[i]
        if (i < charArray.size() - 2 && c >= 'a' && c <= 'h' && c + 1 == charArray[i + 1] && c + 2 == charArray[i + 2]) {
            hasStraight = true
        }
        if (c == lastChar) {
            if (!pairs.contains(c)) {
                pairs.add(c)
                if (pairs.size() == 2) {
                    hasPair = true
                }
            }
        }
        if (c == 'i' || c == 'l' || c == 'o') {
            return false
        }
        lastChar = c
    }
    return hasStraight && hasPair
}

def incrementPassword(String password) {
    def charArray = password.toCharArray()
    for (int i = charArray.size() - 1; i >= 0; i--) {
        if (charArray[i] == 'z') {
            charArray[i] = 'a'
        } else {
            charArray[i]++
            break
        }
    }
    return new String(charArray)
}

def newPassword = inputString
while (!isValidPassword(newPassword)) {
    newPassword = incrementPassword(newPassword)
}

println newPassword

newPassword = incrementPassword(newPassword)
while (!isValidPassword(newPassword)) {
    newPassword = incrementPassword(newPassword)
}

println newPassword