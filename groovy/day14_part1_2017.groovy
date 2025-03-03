
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

def knotHash(String input) {
    def lengths = input.bytes.toList() + [17, 31, 73, 47, 23]
    def list = (0..255).toList()
    def currentPosition = 0
    def skipSize = 0

    1.upto(64) {
        lengths.each { length ->
            if (length > list.size()) {
                return
            }

            def subList = []
            for (int i = 0; i < length; i++) {
                subList.add(list[(currentPosition + i) % list.size()])
            }
            subList = subList.reverse()

            for (int i = 0; i < length; i++) {
                list[(currentPosition + i) % list.size()] = subList[i]
            }

            currentPosition = (currentPosition + length + skipSize) % list.size()
            skipSize++
        }
    }

    def denseHash = []
    for (int i = 0; i < 16; i++) {
        def xor = 0
        for (int j = 0; j < 16; j++) {
            xor ^= list[i * 16 + j]
        }
        denseHash.add(xor)
    }

    def hexHash = denseHash.collect { String.format("%02x", it) }.join()
    return hexHash
}


def hexToBinary(String hex) {
    def binary = new StringBuilder()
    hex.each {
        def decimal = Integer.parseInt(it.toString(), 16)
        def binaryString = Integer.toBinaryString(decimal)
        while (binaryString.length() < 4) {
            binaryString = "0" + binaryString
        }
        binary.append(binaryString)
    }
    return binary.toString()
}

def solve(String key) {
    def grid = []
    for (int i = 0; i < 128; i++) {
        def input = key + "-" + i
        def hash = knotHash(input)
        def binary = hexToBinary(hash)
        grid.add(binary)
    }

    def usedSquares = 0
    grid.each { row ->
        row.each { bit ->
            if (bit == '1') {
                usedSquares++
            }
        }
    }
    return usedSquares
}

def main() {
    def key = new File("input.txt").text.trim()
    def result = solve(key)
    println result
}

main()
