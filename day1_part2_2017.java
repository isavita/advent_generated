
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class solution {
    public static void main(String[] args) {
        try {
            String input = Files.readString(Paths.get("input.txt")).trim();
            int halfway = input.length() / 2;
            int sum = 0;

            for (int i = 0; i < input.length(); i++) {
                int next = (i + halfway) % input.length();
                if (input.charAt(i) == input.charAt(next)) {
                    sum += input.charAt(i) - '0';
                }
            }

            System.out.println(sum);
        } catch (IOException e) {
            System.out.println("File reading error" + e);
        }
    }
}
