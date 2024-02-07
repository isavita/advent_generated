
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {
    public static long applyMask(long value, String mask) {
        long result = 0;
        for (int i = 0; i < 36; i++) {
            long bitValue = 1L << (35 - i);
            if (mask.charAt(i) == '1') {
                result |= bitValue;
            } else if (mask.charAt(i) == 'X') {
                result |= (value & bitValue);
            }
        }
        return result;
    }

    public static void main(String[] args) {
        String file = "input.txt";
        String line;
        String mask = "";
        Map<Long, Long> mem = new HashMap<>();
        Pattern pattern = Pattern.compile("mem\\[(\\d+)] = (\\d+)");

        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("mask = ")) {
                    mask = line.substring(7);
                } else {
                    Matcher matcher = pattern.matcher(line);
                    if (matcher.find()) {
                        long address = Long.parseLong(matcher.group(1));
                        long value = Long.parseLong(matcher.group(2));
                        mem.put(address, applyMask(value, mask));
                    }
                }
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            return;
        }

        long sum = 0;
        for (long value : mem.values()) {
            sum += value;
        }

        System.out.println(sum);
    }
}
