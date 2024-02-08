import java.security.MessageDigest

def input = new File("input.txt").text.trim()

def number = 1
def md5 = MessageDigest.getInstance("MD5")

while (true) {
    def hash = md5.digest((input + number).bytes).encodeHex().toString()
    if (hash.startsWith("00000")) {
        println number
        break
    }
    number++
}