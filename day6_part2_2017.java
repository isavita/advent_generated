
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line = reader.readLine();
            List<Integer> banks = new ArrayList<>();
            for (String num : line.trim().split("\\s+")) {
                banks.add(Integer.parseInt(num));
            }
            
            Map<String, Integer> seen = new HashMap<>();
            int cycles = 0;
            
            while (true) {
                String state = banks.toString();
                
                if (seen.containsKey(state)) {
                    System.out.println("The size of the loop is " + (cycles - seen.get(state)));
                    return;
                }
                seen.put(state, cycles);
                
                int maxIndex = 0;
                for (int i = 1; i < banks.size(); i++) {
                    if (banks.get(i) > banks.get(maxIndex)) {
                        maxIndex = i;
                    }
                }
                
                int blocks = banks.get(maxIndex);
                banks.set(maxIndex, 0);
                for (int i = 1; i <= blocks; i++) {
                    banks.set((maxIndex + i) % banks.size(), banks.get((maxIndex + i) % banks.size()) + 1);
                }
                
                cycles++;
            }
        } catch (IOException e) {
            System.out.println("File reading error" + e);
        }
    }
}
