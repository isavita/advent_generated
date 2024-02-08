
def data = new File("input.txt").text.trim()
def keys = 0
def index = 0

while (keys < 64) {
    def hash = getMD5Hash(data + index.toString())
    def triplet = findTriplet(hash)
    
    if (triplet != "") {
        for (int i = 1; i <= 1000; i++) {
            def nextHash = getMD5Hash(data + (index + i).toString())
            if (nextHash.contains(triplet * 5)) {
                keys++
                break
            }
        }
    }
    
    index++
}

println(index - 1)

String getMD5Hash(String input) {
    def md = java.security.MessageDigest.getInstance("MD5")
    md.update(input.getBytes())
    md.digest().encodeHex().toString()
}

String findTriplet(String hash) {
    for (int i = 0; i < hash.size() - 2; i++) {
        if (hash[i] == hash[i + 1] && hash[i] == hash[i + 2]) {
            return hash[i] as String
        }
    }
    return ""
}
