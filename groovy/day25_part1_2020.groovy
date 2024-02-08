def input = new File("input.txt").text.split("\n").collect { it.toLong() }

def transform(value, subject) {
    (value * subject) % 20201227
}

def findLoopSize(publicKey) {
    def value = 1
    def subject = 7
    def loopSize = 0

    while (value != publicKey) {
        value = transform(value, subject)
        loopSize++
    }

    loopSize
}

def cardPublicKey = input[0]
def doorPublicKey = input[1]

def cardLoopSize = findLoopSize(cardPublicKey)
def doorLoopSize = findLoopSize(doorPublicKey)

def encryptionKey = 1
for (int i = 0; i < cardLoopSize; i++) {
    encryptionKey = transform(encryptionKey, doorPublicKey)
}

println encryptionKey