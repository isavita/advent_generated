
import java.security.MessageDigest

def doorID = new File("input.txt").text.trim()
def password = new char[8]
def filledPositions = 0
def found = new boolean[8]
def digest = MessageDigest.getInstance("MD5")

for (int i = 0; filledPositions < 8; i++) {
    def hash = digest.digest((doorID + i).bytes).encodeHex().toString()
    if (hash.startsWith("00000")) {
        def pos = hash[5]
        if (pos >= '0' && pos <= '7') {
            def posIndex = pos.toInteger()
            if (!found[posIndex]) {
                found[posIndex] = true
                password[posIndex] = hash[6]
                filledPositions++
            }
        }
    }
}

println new String(password)
