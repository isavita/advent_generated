def input = new File('input.txt').text.trim()
def password = input.toCharArray()

while (true) {
    incrementPassword(password)
    if (isValidPassword(password)) {
        println new String(password)
        break
    }
}

def incrementPassword(char[] password) {
    int i = password.length - 1
    while (i >= 0) {
        if (password[i] == 'z') {
            password[i] = 'a'
            i--
        } else {
            password[i]++
            break
        }
    }
}

def isValidPassword(char[] password) {
    def passwordStr = new String(password)
    if (passwordStr.contains('i') || passwordStr.contains('o') || passwordStr.contains('l')) {
        return false
    }
    
    def pairs = [] as Set
    def hasStraight = false
    for (int i = 0; i < password.length - 1; i++) {
        if (password[i] + 1 == password[i + 1]) {
            if (i < password.length - 2 && password[i] + 2 == password[i + 2]) {
                hasStraight = true
            }
        }
        if (password[i] == password[i + 1]) {
            pairs << password[i]
        }
    }
    return hasStraight && pairs.size() >= 2
}