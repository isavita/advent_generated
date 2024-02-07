
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine().trim();
            reader.close();
            int floor = 0;
            int position = 0;
            for (int i = 0; i < input.length(); i++) {
                char c = input.charAt(i);
                if (c == '(') {
                    floor++;
                } else if (c == ')') {
                    floor--;
                }
                if (floor == -1) {
                    position = i + 1;
                    break;
                }
            }
            System.out.println(position);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
