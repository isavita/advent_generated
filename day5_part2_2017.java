
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        List<Integer> offsets = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                offsets.add(Integer.parseInt(line));
            }
        } catch (IOException e) {
            System.out.println("File reading error" + e);
            return;
        }

        int index = 0;
        int steps = 0;

        while (index >= 0 && index < offsets.size()) {
            int jump = offsets.get(index);

            if (jump >= 3) {
                offsets.set(index, offsets.get(index) - 1);
            } else {
                offsets.set(index, offsets.get(index) + 1);
            }

            index += jump;
            steps++;
        }

        System.out.println(steps);
    }
}
