
import java.io.BufferedReader;
import java.io.FileReader;
import java.security.MessageDigest;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String doorID = br.readLine().trim();
            br.close();
            String password = findPassword(doorID);
            System.out.println(password);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String findPassword(String doorID) {
        StringBuilder password = new StringBuilder();
        int i = 0;
        while (password.length() < 8) {
            String hash = md5Hash(doorID + i);
            if (hash.startsWith("00000")) {
                password.append(hash.charAt(5));
            }
            i++;
        }
        return password.toString();
    }

    public static String md5Hash(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] messageDigest = md.digest(input.getBytes());
            StringBuilder hexString = new StringBuilder();
            for (byte b : messageDigest) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) {
                    hexString.append('0');
                }
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (Exception e) {
            e.printStackTrace();
            return "";
        }
    }
}
