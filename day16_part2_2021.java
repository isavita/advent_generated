
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class solution {
    public static void main(String[] args) {
        try {
            String hexStr = Files.readString(Paths.get("input.txt")).trim();
            String binStr = hexToBin(hexStr);
            String[] result = parsePacket(binStr, 0);
            System.out.println(result[2]);
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

    public static String[] parsePacket(String binStr, int idx) {
        int version = (binStr.charAt(idx) - '0') << 2 | (binStr.charAt(idx + 1) - '0') << 1 | (binStr.charAt(idx + 2) - '0');
        int typeID = (binStr.charAt(idx + 3) - '0') << 2 | (binStr.charAt(idx + 4) - '0') << 1 | (binStr.charAt(idx + 5) - '0');
        idx += 6;

        if (typeID == 4) {
            long value = 0;
            while (binStr.charAt(idx) == '1') {
                value = (value << 4) | (binStr.charAt(idx + 1) - '0') << 3 | (binStr.charAt(idx + 2) - '0') << 2 | (binStr.charAt(idx + 3) - '0') << 1 | (binStr.charAt(idx + 4) - '0');
                idx += 5;
            }
            value = (value << 4) | (binStr.charAt(idx + 1) - '0') << 3 | (binStr.charAt(idx + 2) - '0') << 2 | (binStr.charAt(idx + 3) - '0') << 1 | (binStr.charAt(idx + 4) - '0');
            idx += 5;
            return new String[]{String.valueOf(version), String.valueOf(idx), String.valueOf(value)};
        }

        int lengthTypeID = binStr.charAt(idx) - '0';
        idx++;
        int numSubPackets = 0;
        int subPacketLength = 0;

        if (lengthTypeID == 0) {
            subPacketLength = 0;
            for (int i = 0; i < 15; i++) {
                subPacketLength = (subPacketLength << 1) | (binStr.charAt(idx) - '0');
                idx++;
            }
        } else {
            numSubPackets = 0;
            for (int i = 0; i < 11; i++) {
                numSubPackets = (numSubPackets << 1) | (binStr.charAt(idx) - '0');
                idx++;
            }
        }

        long[] values = new long[0];
        while (true) {
            if (lengthTypeID == 0 && subPacketLength == 0) {
                break;
            }
            if (lengthTypeID == 1 && numSubPackets == 0) {
                break;
            }
            String[] subResult = parsePacket(binStr, idx);
            long[] temp = new long[values.length + 1];
            System.arraycopy(values, 0, temp, 0, values.length);
            temp[values.length] = Long.parseLong(subResult[2]);
            values = temp;

            if (lengthTypeID == 0) {
                subPacketLength -= Integer.parseInt(subResult[1]) - idx;
            } else {
                numSubPackets--;
            }
            idx = Integer.parseInt(subResult[1]);
        }

        long result = 0;
        switch (typeID) {
            case 0:
                result = 0;
                for (long value : values) {
                    result += value;
                }
                break;
            case 1:
                result = 1;
                for (long value : values) {
                    result *= value;
                }
                break;
            case 2:
                result = values[0];
                for (long value : values) {
                    if (value < result) {
                        result = value;
                    }
                }
                break;
            case 3:
                result = values[0];
                for (long value : values) {
                    if (value > result) {
                        result = value;
                    }
                }
                break;
            case 5:
                result = 0;
                if (values[0] > values[1]) {
                    result = 1;
                }
                break;
            case 6:
                result = 0;
                if (values[0] < values[1]) {
                    result = 1;
                }
                break;
            case 7:
                result = 0;
                if (values[0] == values[1]) {
                    result = 1;
                }
                break;
            default:
                throw new IllegalArgumentException("Unknown typeID");
        }

        return new String[]{String.valueOf(version), String.valueOf(idx), String.valueOf(result)};
    }
}
