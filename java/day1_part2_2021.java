import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) throws IOException {
        List<Integer> vals = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (!line.isEmpty()) {
                    vals.add(Integer.parseInt(line));
                }
            }
        }

        int prevSum = vals.get(0) + vals.get(1) + vals.get(2);
        int count = 0;
        for (int i = 3; i < vals.size(); i++) {
            int currSum = vals.get(i - 2) + vals.get(i - 1) + vals.get(i);
            if (currSum > prevSum) {
                count++;
            }
            prevSum = currSum;
        }

        System.out.println(count);
    }
}