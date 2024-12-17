
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

class Solution {

    static class FileSegment {
        int id;
        int start;
        int end;

        FileSegment(int id, int start, int end) {
            this.id = id;
            this.start = start;
            this.end = end;
        }
    }

    public static void main(String[] args) throws IOException {
        String line = Files.readString(Paths.get("input.txt")).trim();
        int diskSize = 0;
        for (int i = 0; i < line.length(); i++) {
            diskSize += line.charAt(i) - '0';
        }
        char[] disk = new char[diskSize];
        int fileID = 0;
        boolean isFile = true;
        int diskIndex = 0;
        for (int i = 0; i < line.length(); i++) {
            int length = line.charAt(i) - '0';
            if (isFile) {
                for (int j = 0; j < length; j++) {
                    disk[diskIndex++] = (char) ('0' + fileID);
                }
                fileID++;
            } else {
                for (int j = 0; j < length; j++) {
                    disk[diskIndex++] = '.';
                }
            }
            isFile = !isFile;
        }

        List<FileSegment> files = new ArrayList<>();
        {
            int curID = -1;
            int start = -1;
            for (int i = 0; i < disk.length; i++) {
                if (disk[i] == '.') {
                    curID = -1;
                    continue;
                }
                int id = disk[i] - '0';
                if (id != curID) {
                    curID = id;
                    start = i;
                }
                if (i == disk.length - 1 || (i + 1 < disk.length && disk[i + 1] - '0' != id)) {
                    files.add(new FileSegment(id, start, i));
                }
            }
        }

        for (int i = files.size() - 1; i >= 0; i--) {
            FileSegment f = files.get(i);
            int fileLen = f.end - f.start + 1;
            int leftmostSpan = -1;
            int spanLen = 0;
            for (int j = 0; j < f.start; j++) {
                if (disk[j] == '.') {
                    if (spanLen == 0) {
                        leftmostSpan = j;
                    }
                    spanLen++;
                    if (spanLen == fileLen) {
                        break;
                    }
                } else {
                    spanLen = 0;
                    leftmostSpan = -1;
                }
            }
            if (leftmostSpan != -1 && spanLen == fileLen) {
                for (int x = f.start; x <= f.end; x++) {
                    disk[x] = '.';
                }
                for (int x = 0; x < fileLen; x++) {
                    disk[leftmostSpan + x] = (char) ('0' + f.id);
                }
            }
        }

        long checksum = 0;
        for (int i = 0; i < disk.length; i++) {
            if (disk[i] != '.') {
                int id = disk[i] - '0';
                checksum += (long) i * id;
            }
        }
        System.out.println(checksum);
    }
}
