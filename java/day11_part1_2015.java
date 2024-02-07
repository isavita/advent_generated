
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {

    public static void main(String[] args) {
        String currentPassword = readInput("input.txt");
        String newPassword = findNextPassword(currentPassword);
        System.out.println(newPassword);
    }

    public static String readInput(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            return br.readLine();
        } catch (IOException e) {
            return "";
        }
    }

    public static String findNextPassword(String password) {
        while (true) {
            password = incrementPassword(password);
            if (isValidPassword(password)) {
                break;
            }
        }
        return password;
    }

    public static String incrementPassword(String password) {
        char[] chars = password.toCharArray();
        for (int i = chars.length - 1; i >= 0; i--) {
            chars[i]++;
            if (chars[i] > 'z') {
                chars[i] = 'a';
            } else {
                break;
            }
        }
        return new String(chars);
    }

    public static boolean isValidPassword(String password) {
        return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password);
    }

    public static boolean hasStraight(String password) {
        for (int i = 0; i < password.length() - 2; i++) {
            if (password.charAt(i) + 1 == password.charAt(i + 1) && password.charAt(i) + 2 == password.charAt(i + 2)) {
                return true;
            }
        }
        return false;
    }

    public static boolean containsInvalidLetters(String password) {
        for (char c : password.toCharArray()) {
            if (c == 'i' || c == 'o' || c == 'l') {
                return true;
            }
        }
        return false;
    }

    public static boolean hasTwoPairs(String password) {
        int count = 0;
        for (int i = 0; i < password.length() - 1; i++) {
            if (password.charAt(i) == password.charAt(i + 1)) {
                count++;
                i++; // Skip the next character
            }
        }
        return count >= 2;
    }
}
