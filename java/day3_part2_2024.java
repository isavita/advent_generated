
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("input.txt"));
        Pattern pattern = Pattern.compile("(mul\\((\\d{1,3}),(\\d{1,3})\\))|(do\\(\\))|(don't\\(\\))");
        Matcher matcher = pattern.matcher(input);

        boolean enabled = true;
        int totalSum = 0;

        while (matcher.find()) {
            if (matcher.group(1) != null) {
                if (enabled) {
                    int x = Integer.parseInt(matcher.group(2));
                    int y = Integer.parseInt(matcher.group(3));
                    totalSum += x * y;
                }
            } else if (matcher.group(4) != null) {
                enabled = true;
            } else if (matcher.group(5) != null) {
                enabled = false;
            }
        }
        System.out.println(totalSum);
    }
}
