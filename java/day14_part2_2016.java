
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

public class solution {

    private static final Map<String, String> hashCache = new HashMap<>();

    public static void main(String[] args) throws IOException, NoSuchAlgorithmException {
        String salt = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
        int keys = 0;
        int index = 0;
        while (keys < 64) {
            String hash = getStretchedMD5Hash(salt + index);
            String triplet = findTriplet(hash);
            if (!triplet.isEmpty()) {
                for (int i = 1; i <= 1000; i++) {
                    String nextHash = getStretchedMD5Hash(salt + (index + i));
                    if (nextHash.contains(new String(new char[5]).replace('\0', triplet.charAt(0)))) {
                        keys++;
                        break;
                    }
                }
            }
            index++;
        }

        System.out.println(index - 1);
    }

    private static String getStretchedMD5Hash(String input) throws NoSuchAlgorithmException {
        if (hashCache.containsKey(input)) {
            return hashCache.get(input);
        }
        String hash = getMD5Hash(input);
        for (int i = 0; i < 2016; i++) {
            hash = getMD5Hash(hash);
        }
        hashCache.put(input, hash);
        return hash;
    }

    private static String getMD5Hash(String input) throws NoSuchAlgorithmException {
        MessageDigest digest = MessageDigest.getInstance("MD5");
        byte[] hash = digest.digest(input.getBytes());
        StringBuilder hexString = new StringBuilder();
        for (byte b : hash) {
            String hex = Integer.toHexString(0xff & b);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }

    private static String findTriplet(String hash) {
        for (int i = 0; i < hash.length() - 2; i++) {
            if (hash.charAt(i) == hash.charAt(i + 1) && hash.charAt(i) == hash.charAt(i + 2)) {
                return String.valueOf(hash.charAt(i));
            }
        }
        return "";
    }
}
