import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Main {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String doorId = br.readLine();
            char[] password = new char[8];
            int index = 0;
            int found = 0;

            while (found < 8) {
                String hashInput = doorId + index;
                String hash = getMD5(hashInput);

                if (hash.startsWith("00000")) {
                    int position = Integer.parseInt(hash.substring(5, 6), 16);
                    if (position >= 0 && position < 8) {
                        if (password[position] == 0) {
                            password[position] = hash.charAt(6);
                            found++;
                        }
                    }
                }

                index++;
            }

            System.out.println("The password is: " + new String(password));
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }

    public static String getMD5(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] bytes = md.digest(input.getBytes());
            StringBuilder sb = new StringBuilder();

            for (byte b : bytes) {
                String hex = Integer.toHexString(b & 0xff);
                if (hex.length() == 1) {
                    sb.append("0");
                }
                sb.append(hex);
            }

            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            System.out.println("MD5 algorithm not found: " + e.getMessage());
            return null;
        }
    }
}