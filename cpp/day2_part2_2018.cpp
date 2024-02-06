
#include <iostream>
#include <fstream>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    for (int i = 0; i < lines.size() - 1; i++) {
        for (int j = i + 1; j < lines.size(); j++) {
            int diff = 0;
            for (int k = 0; k < lines[i].size(); k++) {
                if (lines[i][k] != lines[j][k]) {
                    diff++;
                    if (diff > 1) {
                        break;
                    }
                }
            }
            if (diff == 1) {
                std::string common = "";
                for (int k = 0; k < lines[i].size(); k++) {
                    if (lines[i][k] == lines[j][k]) {
                        common += lines[i][k];
                    }
                }
                std::cout << common << std::endl;
                return 0;
            }
        }
    }
    return 0;
}
