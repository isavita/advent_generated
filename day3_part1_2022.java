
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class solution {
    public static void main(String[] args) {
        int sum = 0;
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = br.readLine()) != null) {
                int half = line.length() / 2;
                String firstCompartment = line.substring(0, half);
                String secondCompartment = line.substring(half);

                HashMap<Character, Integer> compartmentMap = new HashMap<>();
                for (char item : firstCompartment.toCharArray()) {
                    compartmentMap.put(item, compartmentMap.getOrDefault(item, 0) + 1);
                }
                for (char item : secondCompartment.toCharArray()) {
                    if (compartmentMap.containsKey(item)) {
                        sum += itemPriority(item);
                        break;
                    }
                }
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        System.out.println(sum);
    }

    private static int itemPriority(char item) {
        if (item >= 'a' && item <= 'z') {
            return item - 'a' + 1;
        }
        return item - 'A' + 27;
    }
}
