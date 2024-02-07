
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int steps = Integer.parseInt(reader.readLine().trim());
            List<Integer> buffer = new ArrayList<>();
            buffer.add(0);
            int currentPos = 0;

            for (int i = 1; i <= 2017; i++) {
                currentPos = (currentPos + steps) % buffer.size();
                buffer.add(currentPos + 1, i);
                currentPos++;
            }

            int answer = 0;
            for (int i = 0; i < buffer.size(); i++) {
                if (buffer.get(i) == 2017) {
                    answer = buffer.get((i + 1) % buffer.size());
                    break;
                }
            }

            System.out.println(answer);

            reader.close();
        } catch (IOException e) {
            System.out.println("File reading error: " + e.getMessage());
        }
    }
}
