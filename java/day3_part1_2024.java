
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MulSum {

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("input.txt"));
        Pattern pattern = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)");
        Matcher matcher = pattern.matcher(input);
        int totalSum = 0;
        while (matcher.find()) {
            int x = Integer.parseInt(matcher.group(1));
            int y = Integer.parseInt(matcher.group(2));
            totalSum += x * y;
        }
        System.out.println(totalSum);
    }
}
