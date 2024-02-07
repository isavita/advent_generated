
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine();
            reader.close();

            int[] lengths = new int[input.length()];
            for (int i = 0; i < input.length(); i++) {
                lengths[i] = (int) input.charAt(i);
            }
            int[] extraLengths = {17, 31, 73, 47, 23};
            int[] combinedLengths = new int[lengths.length + extraLengths.length];
            System.arraycopy(lengths, 0, combinedLengths, 0, lengths.length);
            System.arraycopy(extraLengths, 0, combinedLengths, lengths.length, extraLengths.length);

            int[] list = new int[256];
            for (int i = 0; i < 256; i++) {
                list[i] = i;
            }
            int currentPosition = 0;
            int skipSize = 0;

            for (int round = 0; round < 64; round++) {
                for (int length : combinedLengths) {
                    for (int i = 0; i < length / 2; i++) {
                        int start = (currentPosition + i) % 256;
                        int end = (currentPosition + length - 1 - i) % 256;
                        int temp = list[start];
                        list[start] = list[end];
                        list[end] = temp;
                    }
                    currentPosition = (currentPosition + length + skipSize) % 256;
                    skipSize++;
                }
            }

            byte[] denseHash = new byte[16];
            for (int i = 0; i < 256; i += 16) {
                int xor = 0;
                for (int j = 0; j < 16; j++) {
                    xor ^= list[i + j];
                }
                denseHash[i/16] = (byte) xor;
            }

            StringBuilder hexHash = new StringBuilder();
            for (byte b : denseHash) {
                hexHash.append(String.format("%02x", b));
            }

            System.out.println(hexHash.toString());
        } catch (IOException e) {
            System.out.println("File reading error: " + e.getMessage());
        }
    }
}
