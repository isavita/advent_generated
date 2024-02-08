
def input = new File("input.txt").text.trim()

def index = 0
def password = ""

while (password.size() < 8) {
    def hash = input + index
    def digest = java.security.MessageDigest.getInstance("MD5").digest(hash.bytes).encodeHex().toString()

    if (digest.startsWith("00000")) {
        password += digest[5]
    }

    index++
}

println password
