
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {

    public static long[] generateAddresses(String mask, long address) {
        int[] floating = new int[36];
        int floatingCount = 0;
        long[] addresses = new long[0];

        for (int i = 0; i < mask.length(); i++) {
            char bit = mask.charAt(i);
            if (bit == '1') {
                address |= (1L << (35 - i));
            } else if (bit == 'X') {
                floating[floatingCount++] = 35 - i;
            }
        }

        int count = 1 << floatingCount;
        for (int i = 0; i < count; i++) {
            long modAddress = address;
            for (int j = 0; j < floatingCount; j++) {
                int pos = floating[j];
                if ((i & (1 << j)) == 0) {
                    modAddress &= ~(1L << pos);
                } else {
                    modAddress |= (1L << pos);
                }
            }
            addresses = insertIntoArray(addresses, modAddress);
        }

        return addresses;
    }

    public static long[] insertIntoArray(long[] arr, long element) {
        long[] newArray = new long[arr.length + 1];
        System.arraycopy(arr, 0, newArray, 0, arr.length);
        newArray[arr.length] = element;
        return newArray;
    }

    public static void main(String[] args) {
        File file = new File("input.txt");
        try {
            Scanner scanner = new Scanner(file);
            String mask = "";
            Map<Long, Long> mem = new HashMap<>();
            Pattern pattern = Pattern.compile("mem\\[(\\d+)] = (\\d+)");

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                if (line.startsWith("mask = ")) {
                    mask = line.substring(7);
                } else {
                    Matcher matcher = pattern.matcher(line);
                    if (matcher.find()) {
                        long address = Long.parseLong(matcher.group(1));
                        long value = Long.parseLong(matcher.group(2));
                        long[] addresses = generateAddresses(mask, address);
                        for (long addr : addresses) {
                            mem.put(addr, value);
                        }
                    }
                }
            }

            long sum = 0;
            for (long value : mem.values()) {
                sum += value;
            }

            System.out.println(sum);

        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e.getMessage());
        }
    }
}
