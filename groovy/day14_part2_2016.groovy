
import java.security.MessageDigest
import java.io.File

class Solution {

    static Map hashCache = [:]

    static void main(String[] args) {
        String salt = new File('input.txt').text.trim()

        int keys = 0
        int index = 0

        while (keys < 64) {
            String hash = getStretchedMD5Hash(salt + index)
            String triplet = findTriplet(hash)

            if (triplet) {
                for (int i = 1; i <= 1000; i++) {
                    String nextHash = getStretchedMD5Hash(salt + (index + i))
                    if (nextHash.contains(triplet.repeat(5))) {
                        keys++
                        break
                    }
                }
            }
            index++
        }

        println(index - 1)
    }

    static String getStretchedMD5Hash(String input) {
        if (hashCache.containsKey(input)) {
            return hashCache.get(input)
        }

        String hash = getMD5Hash(input)
        for (int i = 0; i < 2016; i++) {
            hash = getMD5Hash(hash)
        }

        hashCache.put(input, hash)
        return hash
    }

    static String getMD5Hash(String input) {
        MessageDigest md = MessageDigest.getInstance("MD5")
        byte[] hashBytes = md.digest(input.bytes)
        StringBuilder sb = new StringBuilder()
        for (byte b : hashBytes) {
            sb.append(String.format("%02x", b))
        }
        return sb.toString()
    }

    static String findTriplet(String hash) {
        for (int i = 0; i < hash.length() - 2; i++) {
            if (hash.charAt(i) == hash.charAt(i + 1) && hash.charAt(i) == hash.charAt(i + 2)) {
                return hash.charAt(i) as String
            }
        }
        return ""
    }
}
