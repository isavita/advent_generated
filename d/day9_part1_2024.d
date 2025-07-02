
import std.stdio;
import std.file;
import std.string;
import std.algorithm.searching;

void main() {
    auto data = readText("input.txt").strip();

    long totalSize = 0;
    foreach (c; data) {
        totalSize += c - '0';
    }
    auto disk = new int[totalSize];

    bool isFile = true;
    int fileID = 0;
    size_t k = 0;
    foreach (c; data) {
        int length = c - '0';
        if (isFile) {
            foreach (j; 0 .. length) disk[k++] = fileID;
            fileID++;
        } else {
            foreach (j; 0 .. length) disk[k++] = -1;
        }
        isFile = !isFile;
    }

    while (true) {
        auto lfree = disk.countUntil(-1);
        if (lfree == -1) break;

        ptrdiff_t rfile = -1;
        for (ptrdiff_t i = disk.length - 1; i > lfree; --i) {
            if (disk[i] != -1) {
                rfile = i;
                break;
            }
        }

        if (rfile == -1) break;

        disk[lfree] = disk[rfile];
        disk[rfile] = -1;
    }

    long checksum = 0;
    foreach (i, val; disk) {
        if (val != -1) {
            checksum += i * val;
        }
    }
    writeln(checksum);
}
