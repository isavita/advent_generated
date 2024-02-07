
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String hexStr = reader.readLine().trim();
            String binStr = hexToBin(hexStr);
            int[] result = parsePacket(binStr, 0);
            System.out.println(result[0]);
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
    }

    public static String hexToBin(String hex) {
        StringBuilder bin = new StringBuilder();
        for (char h : hex.toCharArray()) {
            int b = Integer.parseInt(String.valueOf(h), 16);
            bin.append(String.format("%04d", Integer.parseInt(Integer.toBinaryString(b))));
        }
        return bin.toString();
    }

    public static int[] parsePacket(String binStr, int idx) {
        int version = (binStr.charAt(idx) - '0') << 2 | (binStr.charAt(idx + 1) - '0') << 1 | (binStr.charAt(idx + 2) - '0');
        int typeID = (binStr.charAt(idx + 3) - '0') << 2 | (binStr.charAt(idx + 4) - '0') << 1 | (binStr.charAt(idx + 5) - '0');
        idx += 6;

        if (typeID == 4) {
            while (binStr.charAt(idx) == '1') {
                idx += 5;
            }
            idx += 5;
            return new int[]{version, idx};
        }

        int lengthTypeID = binStr.charAt(idx) - '0';
        idx++;
        int numSubPackets = 0;
        int subPacketLength = 0;

        if (lengthTypeID == 0) {
            subPacketLength = 0;
            for (int i = 0; i < 15; i++) {
                subPacketLength = subPacketLength << 1 | (binStr.charAt(idx) - '0');
                idx++;
            }
        } else {
            numSubPackets = 0;
            for (int i = 0; i < 11; i++) {
                numSubPackets = numSubPackets << 1 | (binStr.charAt(idx) - '0');
                idx++;
            }
        }

        int versionSum = version;
        while (true) {
            if (lengthTypeID == 0 && subPacketLength == 0) {
                break;
            }
            if (lengthTypeID == 1 && numSubPackets == 0) {
                break;
            }
            int[] subResult = parsePacket(binStr, idx);
            versionSum += subResult[0];

            if (lengthTypeID == 0) {
                subPacketLength -= subResult[1] - idx;
            } else {
                numSubPackets--;
            }
            idx = subResult[1];
        }
        return new int[]{versionSum, idx};
    }
}
