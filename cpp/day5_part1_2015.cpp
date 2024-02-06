#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string input;
    std::string line;
    while (std::getline(file, line)) {
        input += line + "\n";
    }
    file.close();

    int nice = 0;
    std::regex disallowPattern("(ab|cd|pq|xy)");
    std::vector<std::string> lines;
    size_t start = 0;
    size_t end = input.find("\n");
    while (end != std::string::npos) {
        lines.push_back(input.substr(start, end - start));
        start = end + 1;
        end = input.find("\n", start);
    }

    for (const std::string& line : lines) {
        int vowels = 0;
        for (char c : line) {
            if (std::string("aeiou").find(c) != std::string::npos) {
                vowels++;
            }
        }

        bool hasDouble = false;
        for (size_t i = 0; i < line.size() - 1; i++) {
            if (line[i] == line[i + 1]) {
                hasDouble = true;
                break;
            }
        }

        if (vowels >= 3 && !std::regex_search(line, disallowPattern) && hasDouble) {
            nice++;
        }
    }

    std::cout << nice << std::endl;

    return 0;
}