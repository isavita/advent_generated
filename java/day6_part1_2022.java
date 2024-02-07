
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;

public class solution {
    public static void main(String[] args) {
        String s = readAll("input.txt");
        System.out.println(firstNUnique(s, 4));
    }

    public static int firstNUnique(String s, int n) {
        for (int i = n; i < s.length(); i++) {
            String sub = s.substring(i - n, i);
            if (sub.length() == setOf(sub).size()) {
                return i;
            }
        }
        return -1;
    }

    public static String readAll(String path) {
        StringBuilder content = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            String line;
            while ((line = br.readLine()) != null) {
                content.append(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return content.toString().trim();
    }

    public static HashSet<Character> setOf(String s) {
        HashSet<Character> set = new HashSet<>();
        for (char c : s.toCharArray()) {
            set.add(c);
        }
        return set;
    }
}
