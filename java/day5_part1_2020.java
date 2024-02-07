
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {
    public static void main(String[] args) {
        int maxSeatID = 0;
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = br.readLine()) != null) {
                String pass = line.replace("F", "0").replace("B", "1").replace("L", "0").replace("R", "1");
                int seatID = decode(pass);
                if (seatID > maxSeatID) {
                    maxSeatID = seatID;
                }
            }
            br.close();
        } catch (IOException e) {
            System.out.println("Error opening file: " + e);
        }

        System.out.println(maxSeatID);
    }

    public static int decode(String pass) {
        int row = binaryToInt(pass.substring(0, 7));
        int column = binaryToInt(pass.substring(7));
        return row * 8 + column;
    }

    public static int binaryToInt(String binaryStr) {
        int result = 0;
        for (int i = 0; i < binaryStr.length(); i++) {
            if (binaryStr.charAt(i) == '1') {
                result |= 1 << (binaryStr.length() - i - 1);
            }
        }
        return result;
    }
}
