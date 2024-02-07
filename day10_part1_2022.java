
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        List<Integer> x = new ArrayList<>();
        x.add(1);

        try {
            List<String> lines = Files.readAllLines(Paths.get("input.txt"));
            for (String line : lines) {
                switch (line) {
                    case "noop":
                        x.add(x.get(x.size()-1));
                        break;
                    default:
                        int n = Integer.parseInt(line.split(" ")[1]);
                        x.add(x.get(x.size()-1));
                        x.add(x.get(x.size()-1) + n);
                }
            }

            int sum = 0;
            for (int i = 0; i < x.size(); i++) {
                if ((i-19) % 40 == 0) {
                    sum += (i + 1) * x.get(i);
                }
            }
            System.out.println(sum);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
