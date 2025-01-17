
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Snailfish {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
            reader.close();

            String sum = lines.get(0);
            for (int i = 1; i < lines.size(); i++) {
                sum = add(sum, lines.get(i));
            }

            System.out.println(magnitude(sum));

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static String add(String s1, String s2) {
        String result = "[" + s1 + "," + s2 + "]";
        return reduce(result);
    }

    static String reduce(String s) {
        while (true) {
            String exploded = explode(s);
            if (!exploded.equals(s)) {
                s = exploded;
                continue;
            }
            String split = split(s);
            if (!split.equals(s)) {
                s = split;
                continue;
            }
            break;
        }
        return s;
    }

    static String explode(String s) {
        int depth = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '[') {
                depth++;
            } else if (c == ']') {
                depth--;
            } else if (depth > 4 && Character.isDigit(c)) {
                int j = i + 1;
                while (Character.isDigit(s.charAt(j))) {
                    j++;
                }
                if (s.charAt(j) == ',') {
                    int k = j + 1;
                    while (Character.isDigit(s.charAt(k))) {
                        k++;
                    }
                    if (s.charAt(k) == ']') {
                        int left = Integer.parseInt(s.substring(i, j));
                        int right = Integer.parseInt(s.substring(j + 1, k));
                        String prefix = s.substring(0, i - 1);
                        String suffix = s.substring(k + 1);

                        String newPrefix = addToRightmost(prefix, left);
                        String newSuffix = addToLeftmost(suffix, right);

                        return newPrefix + "0" + newSuffix;
                    }
                }
            }
        }
        return s;
    }
    
    static String addToRightmost(String s, int num) {
        for (int i = s.length() - 1; i >= 0; i--) {
            if (Character.isDigit(s.charAt(i))) {
                int j = i;
                while (j >= 0 && Character.isDigit(s.charAt(j))) {
                    j--;
                }
                int n = Integer.parseInt(s.substring(j + 1, i + 1));
                return s.substring(0, j + 1) + (n + num) + s.substring(i + 1);
            }
        }
        return s;
    }

    static String addToLeftmost(String s, int num) {
        for (int i = 0; i < s.length(); i++) {
            if (Character.isDigit(s.charAt(i))) {
                int j = i + 1;
                while (j < s.length() && Character.isDigit(s.charAt(j))) {
                    j++;
                }
                int n = Integer.parseInt(s.substring(i, j));
                return s.substring(0, i) + (n + num) + s.substring(j);
            }
        }
        return s;
    }

    static String split(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (Character.isDigit(s.charAt(i))) {
                int j = i + 1;
                while (j < s.length() && Character.isDigit(s.charAt(j))) {
                    j++;
                }
                int num = Integer.parseInt(s.substring(i, j));
                if (num >= 10) {
                    int left = num / 2;
                    int right = (num + 1) / 2;
                    return s.substring(0, i) + "[" + left + "," + right + "]" + s.substring(j);
                }
            }
        }
        return s;
    }

    static int magnitude(String s) {
        if (!s.startsWith("[")) {
            return Integer.parseInt(s);
        }
        int depth = 0;
        int commaIndex = -1;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '[') {
                depth++;
            } else if (c == ']') {
                depth--;
            } else if (c == ',' && depth == 1) {
                commaIndex = i;
                break;
            }
        }
        
        return 3 * magnitude(s.substring(1, commaIndex)) + 2 * magnitude(s.substring(commaIndex + 1, s.length() - 1));
    }
}
