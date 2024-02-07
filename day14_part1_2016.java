
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

public class solution {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String salt = reader.readLine().trim();
            int keys = 0;
            int index = 0;
            while (keys < 64) {
                String hash = getMD5Hash(salt + index);
                String triplet = findTriplet(hash);
                if (!triplet.equals("")) {
                    for (int i = 1; i <= 1000; i++) {
                        String nextHash = getMD5Hash(salt + (index + i));
                        if (nextHash.contains(triplet.repeat(5))) {
                            keys++;
                            break;
                        }
                    }
                }
                index++;
            }
            System.out.println(index - 1);
            reader.close();
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    public static String getMD5Hash(String input) throws NoSuchAlgorithmException {
        MessageDigest md = MessageDigest.getInstance("MD5");
        byte[] hashInBytes = md.digest(input.getBytes());
        StringBuilder sb = new StringBuilder();
        for (byte b : hashInBytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    public static String findTriplet(String hash) {
        for (int i = 0; i < hash.length() - 2; i++) {
            if (hash.charAt(i) == hash.charAt(i + 1) && hash.charAt(i) == hash.charAt(i + 2)) {
                return String.valueOf(hash.charAt(i));
            }
        }
        return "";
    }
}
