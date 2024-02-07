
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        int totalDiff = 0;
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                int originalLength = line.length();
                int encodedLength = calculateEncodedLength(line);
                totalDiff += encodedLength - originalLength;
            }
            System.out.println(totalDiff);
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }

    public static int calculateEncodedLength(String s) {
        StringBuilder encoded = new StringBuilder("\"");
        for (char ch : s.toCharArray()) {
            if (ch == '\\' || ch == '"') {
                encoded.append("\\");
            }
            encoded.append(ch);
        }
        encoded.append("\"");
        return encoded.length();
    }
}
