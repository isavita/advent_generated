
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class ALU {

    public static void main(String[] args) {
        List<String> instructions = readInstructions("input.txt");
        String largestModelNumber = findLargestModelNumber(instructions);
        System.out.println(largestModelNumber);
    }

    static List<String> readInstructions(String filename) {
        List<String> instructions = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                instructions.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return instructions;
    }

    static String findLargestModelNumber(List<String> instructions) {
        Map<String, Long> registers = new HashMap<>();
        registers.put("w", 0L);
        registers.put("x", 0L);
        registers.put("y", 0L);
        registers.put("z", 0L);

        
        Deque<Pair<Integer, Long>> stack = new ArrayDeque<>();
        long[] digits = new long[14];

        int inputIndex = 0;
        for (int i = 0; i < instructions.size(); i += 18) {
            long a = Long.parseLong(instructions.get(i + 5).split(" ")[2]);
            long b = Long.parseLong(instructions.get(i + 15).split(" ")[2]);

            if (a > 0) {
                stack.push(new Pair<>(inputIndex, b));
            } else {
                Pair<Integer, Long> pair = stack.pop();
                long diff = pair.getValue() + a;
                if (diff >= 0) {
                    digits[pair.getKey()] = 9 - diff;
                    digits[inputIndex] = 9;
                } else {
                    digits[pair.getKey()] = 9;
                    digits[inputIndex] = 9 + diff;
                }
            }
            inputIndex++;
        }

        StringBuilder sb = new StringBuilder();
        for (long digit : digits) {
            sb.append(digit);
        }
        return sb.toString();
    }
    
    static class Pair<K, V> {
        private final K key;
        private final V value;

        public Pair(K key, V value) {
            this.key = key;
            this.value = value;
        }

        public K getKey() {
            return key;
        }

        public V getValue() {
            return value;
        }
    }
}
