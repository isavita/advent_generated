
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

int solve(std::vector<std::string> input) {
    std::vector<Mirror> mirrors;

    std::vector<std::string> mirrorStr;
    for (auto line : input) {
        if (line == "") {
            mirrors.push_back(parseMirror(mirrorStr));
            mirrorStr.clear();
        } else {
            mirrorStr.push_back(line);
        }
    }
    mirrors.push_back(parseMirror(mirrorStr));

    int res = 0;
    for (auto mirror : mirrors) {
        res += getMirrorAxis(mirror.Cols);
        res += getMirrorAxis(mirror.Rows) * 100;
    }
    return res;
}

int main() {
    std::ifstream inputFile("input.txt");
    std::vector<std::string> input;
    std::string line;

    while (std::getline(inputFile, line)) {
        input.push_back(line);
    }

    std::cout << solve(input) << std::endl;

    return 0;
}
