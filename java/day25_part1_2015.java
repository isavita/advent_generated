
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
    public static void main(String[] args) throws IOException {
        String data = new String(Files.readAllBytes(Paths.get("input.txt")));
        Matcher matcher = Pattern.compile("row (\\d+), column (\\d+)").matcher(data);
        if (!matcher.find()) throw new IllegalArgumentException("Invalid input format.");

        int row = Integer.parseInt(matcher.group(1));
        int column = Integer.parseInt(matcher.group(2));

        int pos = getPosition(row, column);
        System.out.println(getCode(pos));
    }

    private static int getPosition(int row, int column) {
        return (row + column - 2) * (row + column - 1) / 2 + column;
    }

    private static int getCode(int position) {
        final int startCode = 20151125;
        final int multiplier = 252533;
        final int modulus = 33554393;

        int code = startCode;
        for (int i = 1; i < position; i++) {
            code = (int) ((long) code * multiplier % modulus);
        }
        return code;
    }
}
