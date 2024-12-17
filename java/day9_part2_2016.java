
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            String input = scanner.nextLine();
            long decompressedLength = getDecompressedLengthV2(input);
            System.out.println(decompressedLength);
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    static long getDecompressedLengthV2(String input) {
        return decompress(input, 0, input.length());
    }

    static long decompress(String input, int start, int end) {
        Pattern markerRegex = Pattern.compile("\\((\\d+)x(\\d+)\\)");
        long length = 0;
        int i = start;
        while (i < end) {
            Matcher matcher = markerRegex.matcher(input.substring(i, end));
            if (matcher.find()) {
                int charCount = Integer.parseInt(matcher.group(1));
                int repeatCount = Integer.parseInt(matcher.group(2));
                int nextIndex = i + matcher.end();
                length += (long) repeatCount * decompress(input, nextIndex, nextIndex + charCount);
                i = nextIndex + charCount;
            } else {
                length++;
                i++;
            }
        }
        return length;
    }
}
