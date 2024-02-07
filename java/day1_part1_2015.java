
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Solution {
    public static void main(String[] args) {
        try {
            String input = Files.readString(Paths.get("input.txt")).trim();
            int floor = 0;
            for (char c : input.toCharArray()) {
                if (c == '(') {
                    floor++;
                } else if (c == ')') {
                    floor--;
                }
            }
            System.out.println(floor);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
