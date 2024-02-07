
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        String[] freqChanges = readInput();
        int freq = 0;
        for (String change : freqChanges) {
            freq += parseChange(change);
        }
        System.out.println(freq);
    }

    public static String[] readInput() {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            StringBuilder input = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                input.append(line).append("\n");
            }
            return input.toString().split("\n");
        } catch (IOException e) {
            e.printStackTrace();
            return new String[0];
        }
    }

    public static int parseChange(String change) {
        String[] signNum = parseSignNum(change);
        int sign = Integer.parseInt(signNum[0]);
        int num = Integer.parseInt(signNum[1]);
        return sign * num;
    }

    public static String[] parseSignNum(String change) {
        int sign = 1;
        if (change.charAt(0) == '-') {
            sign = -1;
            change = change.substring(1);
        }
        return new String[]{String.valueOf(sign), change};
    }
}
