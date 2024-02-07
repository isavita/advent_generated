
import java.io.BufferedReader;
import java.io.FileReader;
import java.security.MessageDigest;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String secretKey = br.readLine().trim();
            br.close();

            int number = 0;
            while (true) {
                String input = secretKey + number;
                MessageDigest md = MessageDigest.getInstance("MD5");
                byte[] hash = md.digest(input.getBytes());
                StringBuilder sb = new StringBuilder();
                for (byte b : hash) {
                    sb.append(String.format("%02x", b));
                }
                String hashString = sb.toString();

                if (hashString.startsWith("000000")) {
                    System.out.println(number);
                    break;
                }
                number++;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
