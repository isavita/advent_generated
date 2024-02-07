
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int[] seatIDs = new int[0];

            String line;
            while ((line = reader.readLine()) != null) {
                String pass = line.replaceAll("F", "0").replaceAll("B", "1")
                                   .replaceAll("L", "0").replaceAll("R", "1");
                int seatID = decode(pass);
                seatIDs = Arrays.copyOf(seatIDs, seatIDs.length + 1);
                seatIDs[seatIDs.length - 1] = seatID;
            }

            Arrays.sort(seatIDs);

            for (int i = 0; i < seatIDs.length - 1; i++) {
                if (seatIDs[i + 1] != seatIDs[i] + 1) {
                    System.out.println(seatIDs[i] + 1);
                    break;
                }
            }

            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
        }
    }

    private static int decode(String pass) {
        int row = binaryToInt(pass.substring(0, 7));
        int column = binaryToInt(pass.substring(7));
        return row * 8 + column;
    }

    private static int binaryToInt(String binaryStr) {
        int result = 0;
        for (int i = 0; i < binaryStr.length(); i++) {
            if (binaryStr.charAt(i) == '1') {
                result |= 1 << (binaryStr.length() - i - 1);
            }
        }
        return result;
    }
}
