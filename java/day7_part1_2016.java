
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int tlsCount = 0;
            String line;
            while ((line = reader.readLine()) != null) {
                if (supportsTLS(line)) {
                    tlsCount++;
                }
            }
            System.out.println(tlsCount);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean supportsTLS(String ip) {
        Pattern insideBrackets = Pattern.compile("\\[[a-z]+\\]");
        Matcher matcher = insideBrackets.matcher(ip);
        while (matcher.find()) {
            if (containsABBA(matcher.group())) {
                return false;
            }
        }

        ip = insideBrackets.matcher(ip).replaceAll("-");
        return containsABBA(ip);
    }

    public static boolean containsABBA(String s) {
        for (int i = 0; i < s.length() - 3; i++) {
            if (s.charAt(i) != s.charAt(i + 1) && s.charAt(i) == s.charAt(i + 3) && s.charAt(i + 1) == s.charAt(i + 2)) {
                return true;
            }
        }
        return false;
    }
}
