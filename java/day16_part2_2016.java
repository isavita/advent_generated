
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        String initialState = readInitialState("input.txt");
        String data = generateData(initialState, 35651584);
        String checksum = calculateChecksum(data);
        System.out.println("Checksum: " + checksum);
    }

    public static String readInitialState(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            return br.readLine();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String generateData(String initialState, int length) {
        StringBuilder data = new StringBuilder(initialState);
        while (data.length() < length) {
            StringBuilder b = new StringBuilder();
            for (int i = data.length() - 1; i >= 0; i--) {
                if (data.charAt(i) == '0') {
                    b.append('1');
                } else {
                    b.append('0');
                }
            }
            data.append("0").append(b.toString());
        }
        return data.substring(0, length);
    }

    public static String calculateChecksum(String data) {
        while (data.length() % 2 == 0) {
            StringBuilder b = new StringBuilder();
            for (int i = 0; i < data.length(); i += 2) {
                if (data.charAt(i) == data.charAt(i + 1)) {
                    b.append('1');
                } else {
                    b.append('0');
                }
            }
            data = b.toString();
        }
        return data;
    }
}
