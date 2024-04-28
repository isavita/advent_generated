import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Main {
    public static void main(String[] args) {
        Map<String, Integer> mfcsam = new HashMap<>();
        mfcsam.put("children", 3);
        mfcsam.put("cats", 7);
        mfcsam.put("samoyeds", 2);
        mfcsam.put("pomeranians", 3);
        mfcsam.put("akitas", 0);
        mfcsam.put("vizslas", 0);
        mfcsam.put("goldfish", 5);
        mfcsam.put("trees", 3);
        mfcsam.put("cars", 2);
        mfcsam.put("perfumes", 1);

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" ");
                String sueNumber = parts[1].substring(0, parts[1].length() - 1);

                boolean matches = true;
                for (int i = 2; i < parts.length; i += 2) {
                    String item = parts[i].substring(0, parts[i].length() - 1);
                    String countStr = parts[i + 1].replaceAll("\\D+", "");
                    int count = Integer.parseInt(countStr);
                    if (!mfcsam.containsKey(item) || mfcsam.get(item) != count) {
                        matches = false;
                        break;
                    }
                }

                if (matches) {
                    System.out.println(sueNumber);
                    break;
                }
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}