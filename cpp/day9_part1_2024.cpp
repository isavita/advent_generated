
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int main() {
    ifstream inputFile("input.txt");
    string line;
    getline(inputFile, line);

    vector<int> disk;
    int file_id = 0;
    bool is_file = true;

    for (char charVal : line) {
        int length = charVal - '0';
        if (is_file) {
            disk.insert(disk.end(), length, file_id);
            file_id++;
        } else {
            disk.insert(disk.end(), length, -1);
        }
        is_file = !is_file;
    }

    while (true) {
        int lfree = -1;
        for (size_t i = 0; i < disk.size(); ++i) {
            if (disk[i] == -1) {
                lfree = i;
                break;
            }
        }
        if (lfree == -1) break;

        int rfile = -1;
        for (int i = disk.size() - 1; i > lfree; --i) {
            if (disk[i] != -1) {
                rfile = i;
                break;
            }
        }
        if (rfile == -1) break;

        disk[lfree] = disk[rfile];
        disk[rfile] = -1;
    }

    long long checksum = 0;
    for (size_t i = 0; i < disk.size(); ++i) {
        if (disk[i] != -1) {
            checksum += static_cast<long long>(i) * disk[i];
        }
    }

    cout << checksum << endl;

    return 0;
}
