
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    private static void reverseSection(int[] arr, int start, int length) {
        int n = arr.length;
        for (int i = start, j = start + length - 1; i < j; i++, j--) {
            int temp = arr[i % n];
            arr[i % n] = arr[j % n];
            arr[j % n] = temp;
        }
    }

    private static String knotHash(String input) {
        List<Integer> lengths = new ArrayList<>();
        for (char c : input.toCharArray()) {
            lengths.add((int) c);
        }
        lengths.add(17);
        lengths.add(31);
        lengths.add(73);
        lengths.add(47);
        lengths.add(23);

        int[] list = new int[256];
        for (int i = 0; i < 256; i++) {
            list[i] = i;
        }

        int position = 0;
        int skip = 0;
        for (int round = 0; round < 64; round++) {
            for (int length : lengths) {
                reverseSection(list, position, length);
                position += length + skip;
                skip++;
            }
        }

        int[] denseHash = new int[16];
        for (int i = 0; i < 16; i++) {
            int xor = 0;
            for (int j = 0; j < 16; j++) {
                xor ^= list[i * 16 + j];
            }
            denseHash[i] = xor;
        }

        StringBuilder hexHash = new StringBuilder();
        for (int value : denseHash) {
            hexHash.append(String.format("%02x", value));
        }
        return hexHash.toString();
    }

    private static String hexToBinary(String hexStr) {
        StringBuilder binaryStr = new StringBuilder();
        for (char hexDigit : hexStr.toCharArray()) {
            long val = Long.parseLong(String.valueOf(hexDigit), 16);
            binaryStr.append(String.format("%4s", Long.toBinaryString(val)).replace(' ', '0'));
        }
        return binaryStr.toString();
    }

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String keyString = reader.readLine().trim();
            int totalUsed = 0;

            for (int i = 0; i < 128; i++) {
                String rowKey = keyString + "-" + i;
                String hash = knotHash(rowKey);
                String binaryRow = hexToBinary(hash);

                for (char bit : binaryRow.toCharArray()) {
                    if (bit == '1') {
                        totalUsed++;
                    }
                }
            }

            System.out.println(totalUsed);

            reader.close();
        } catch (IOException e) {
            System.out.println("File reading error" + e.getMessage());
        }
    }
}
