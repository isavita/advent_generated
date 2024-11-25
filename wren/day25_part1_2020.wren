
import "io" for File

class ComboBreaker {
    static transform(subjectNumber, loopSize) {
        var value = 1
        for (i in 0...loopSize) {
            value = (value * subjectNumber) % 20201227
        }
        return value
    }

    static findLoopSize(publicKey) {
        var value = 1
        var loopSize = 0
        while (value != publicKey) {
            value = (value * 7) % 20201227
            loopSize = loopSize + 1
        }
        return loopSize
    }

    static main() {
        var lines = File.read("input.txt").trim().split("\n")
        var cardPublicKey = Num.fromString(lines[0])
        var doorPublicKey = Num.fromString(lines[1])

        var cardLoopSize = findLoopSize(cardPublicKey)
        var encryptionKey = transform(doorPublicKey, cardLoopSize)

        System.print(encryptionKey)
    }
}

ComboBreaker.main()
