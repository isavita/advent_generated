
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String secretKey = reader.readLine().trim();
            reader.close();

            int number = 0;
            while (true) {
                String input = secretKey + number;
                MessageDigest md = MessageDigest.getInstance("MD5");
                md.update(input.getBytes());
                byte[] digest = md.digest();
                StringBuffer hashString = new StringBuffer();
                for (byte b : digest) {
                    hashString.append(String.format("%02x", b & 0xff));
                }

                if (hashString.toString().startsWith("00000")) {
                    System.out.printf("%d\n", number);
                    break;
                }
                number++;
            }
        } catch (IOException | NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }
}
