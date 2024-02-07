
import java.util.*;
import java.io.*;

public class solution {
    public static void main(String[] args) {
        String s = readAll("input.txt");
        System.out.println(firstNUnique(s, 14));
    }

    public static int firstNUnique(String s, int n) {
        for (int i = n; i < s.length(); i++) {
            byte[] b = s.substring(i - n, i).getBytes();
            if (b.length == SetOf(b).length) {
                return i;
            }
        }
        return -1;
    }

    public static String readAll(String path) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(path));
            String line = reader.readLine();
            reader.close();
            return line.trim();
        } catch (IOException e) {
            e.printStackTrace();
            return "";
        }
    }

    public static byte[] SetOf(byte[] b) {
        Set<Byte> set = new HashSet<>();
        for (byte c : b) {
            set.add(c);
        }
        byte[] result = new byte[set.size()];
        int index = 0;
        for (byte c : set) {
            result[index++] = c;
        }
        return result;
    }
}
