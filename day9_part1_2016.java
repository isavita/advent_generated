
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            String input = scanner.nextLine();
            int decompressedLength = getDecompressedLength(input);
            System.out.println(decompressedLength);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static int getDecompressedLength(String input) {
        Pattern markerPattern = Pattern.compile("\\((\\d+)x(\\d+)\\)");
        int length = 0;
        for (int i = 0; i < input.length(); ) {
            Matcher matcher = markerPattern.matcher(input.substring(i));
            if (matcher.find()) {
                int charCount = Integer.parseInt(matcher.group(1));
                int repeatCount = Integer.parseInt(matcher.group(2));
                length += charCount * repeatCount;
                i += matcher.end() + charCount;
            } else {
                length++;
                i++;
            }
        }
        return length;
    }
}
