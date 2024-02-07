
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {

    private static final int diskLength = 272;

    public static void main(String[] args) {
        String initialState = readInitialState("input.txt");
        String data = generateData(initialState, diskLength);
        String checksum = calculateChecksum(data);
        System.out.println("Checksum: " + checksum);
    }

    private static String readInitialState(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            return br.readLine();
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException("Failed to read initial state");
        }
    }

    private static String generateData(String initialState, int length) {
        String data = initialState;
        while (data.length() < length) {
            StringBuilder sb = new StringBuilder();
            for (int i = data.length() - 1; i >= 0; i--) {
                if (data.charAt(i) == '0') {
                    sb.append('1');
                } else {
                    sb.append('0');
                }
            }
            data = data + "0" + sb.toString();
        }
        return data.substring(0, length);
    }

    private static String calculateChecksum(String data) {
        while (data.length() % 2 == 0) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < data.length(); i += 2) {
                if (data.charAt(i) == data.charAt(i + 1)) {
                    sb.append('1');
                } else {
                    sb.append('0');
                }
            }
            data = sb.toString();
        }
        return data;
    }
}
