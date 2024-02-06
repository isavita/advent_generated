
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

struct Mirror {
    std::vector<int> Rows;
    std::vector<int> Cols;
};

Mirror parseMirror(std::vector<std::string> mirrorStr) {
    Mirror mirror;
    mirror.Rows.resize(mirrorStr.size());
    mirror.Cols.resize(mirrorStr[0].size());

    for (int y = 0; y < mirrorStr.size(); y++) {
        for (int x = 0; x < mirrorStr[0].size(); x++) {
            mirror.Rows[y] <<= 1;
            mirror.Cols[x] <<= 1;
            if (mirrorStr[y][x] == '#') {
                mirror.Rows[y]++;
                mirror.Cols[x]++;
            }
        }
    }

    return mirror;
}

int getMirrorAxis(std::vector<int> lines) {
    for (int i = 1; i < lines.size(); i++) {
        bool isMirror = true;

        for (int j = 0; isMirror && j < std::min(i, static_cast<int>(lines.size()) - i); j++) {
            if (lines[i - 1 - j] != lines[i + j]) {
                isMirror = false;
            }
        }

        if (isMirror) {
            return i;
        }
    }

    return 0;
}

int getMirrorAxisWithOneSmudge(std::vector<int> lines) {
    for (int i = 1; i < lines.size(); i++) {
        bool isMirror = true;
        int numSmudges = 0;

        for (int j = 0; isMirror && j < std::min(i, static_cast<int>(lines.size()) - i); j++) {
            if (lines[i - 1 - j] != lines[i + j]) {
                if (numSmudges > 0) {
                    isMirror = false;
                } else {
                    int dif = lines[i - 1 - j] ^ lines[i + j];
                    bool isOnlyOneSmudge = (dif & (dif - 1)) == 0;
                    if (isOnlyOneSmudge) {
                        numSmudges++;
                    } else {
                        isMirror = false;
                    }
                }
            }
        }

        if (isMirror && numSmudges == 1) {
            return i;
        }
    }

    return 0;
}

int solve(std::vector<std::string> input) {
    std::vector<Mirror> mirrors;
    std::vector<int> res;

    std::vector<std::string> mirrorStr;
    for (const std::string& line : input) {
        if (line.empty()) {
            mirrors.push_back(parseMirror(mirrorStr));
            mirrorStr.clear();
        } else {
            mirrorStr.push_back(line);
        }
    }
    mirrors.push_back(parseMirror(mirrorStr));

    int totalRes = 0;
    for (const Mirror& mirror : mirrors) {
        totalRes += getMirrorAxisWithOneSmudge(mirror.Cols);
        totalRes += getMirrorAxisWithOneSmudge(mirror.Rows) * 100;
    }
    return totalRes;
}

std::vector<std::string> readFile(std::string fileName) {
    std::ifstream file(fileName);
    std::vector<std::string> input;
    std::string line;
    while (std::getline(file, line)) {
        input.push_back(line);
    }
    file.close();
    return input;
}

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;
    return 0;
}
