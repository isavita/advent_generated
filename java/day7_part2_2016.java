import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
    public static void main(String[] args) throws Exception {
        int sslCount = 0;
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (supportsSSL(line)) {
                    sslCount++;
                }
            }
        }
        System.out.println(sslCount);
    }

    public static boolean supportsSSL(String ip) {
        Pattern insideBrackets = Pattern.compile("\\[[a-z]+\\]");
        String[] bracketContents = insideBrackets.matcher(ip).results().map(mr -> mr.group()).toArray(String[]::new);

        ip = insideBrackets.matcher(ip).replaceAll("-");
        for (String aba : findABAs(ip)) {
            String bab = String.valueOf(new char[]{aba.charAt(1), aba.charAt(0), aba.charAt(1)});
            for (String bracketContent : bracketContents) {
                if (bracketContent.contains(bab)) {
                    return true;
                }
            }
        }

        return false;
    }

    public static List<String> findABAs(String s) {
        List<String> abas = new ArrayList<>();
        for (int i = 0; i < s.length() - 2; i++) {
            if (s.charAt(i) != s.charAt(i + 1) && s.charAt(i) == s.charAt(i + 2)) {
                abas.add(s.substring(i, i + 3));
            }
        }
        return abas;
    }
}