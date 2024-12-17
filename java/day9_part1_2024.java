
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) throws IOException {
        String line = Files.readString(Paths.get("input.txt")).trim();
        List<Character> disk = new ArrayList<>();
        int fileID = 0;
        boolean isFile = true;
        for (int i = 0; i < line.length(); i++) {
            int length = Character.getNumericValue(line.charAt(i));
            if (isFile) {
                char fileChar = (char) ('0' + fileID);
                for (int j = 0; j < length; j++) {
                    disk.add(fileChar);
                }
                fileID++;
            } else {
                for (int j = 0; j < length; j++) {
                    disk.add('.');
                }
            }
            isFile = !isFile;
        }

        int lfree = -1;
        while (true) {
            lfree = -1;
            for (int i = 0; i < disk.size(); i++) {
                if (disk.get(i) == '.') {
                    lfree = i;
                    break;
                }
            }
            if (lfree == -1) {
                break;
            }
            int rfile = -1;
            for (int i = disk.size() - 1; i > lfree; i--) {
                if (disk.get(i) != '.') {
                    rfile = i;
                    break;
                }
            }
            if (rfile == -1) {
                break;
            }
            disk.set(lfree, disk.get(rfile));
            disk.set(rfile, '.');
        }

        long checksum = 0;
        for (int i = 0; i < disk.size(); i++) {
            char b = disk.get(i);
            if (b != '.') {
                int id = b - '0';
                checksum += (long) i * id;
            }
        }
        System.out.println(checksum);
    }
}
