#include <iostream>
#include <fstream>
#include <string>
#include <vector>

bool hasAbba(const std::string& str) {
    for (size_t i = 0; i < str.size() - 3; ++i) {
        if (str[i] == str[i + 3] && str[i + 1] == str[i + 2] && str[i] != str[i + 1]) {
            return true;
        }
    }
    return false;
}

bool hasAbbaInHypernet(const std::string& str) {
    size_t pos = 0;
    while ((pos = str.find('[', pos)) != std::string::npos) {
        size_t endPos = str.find(']', pos);
        if (endPos != std::string::npos) {
            std::string hypernet = str.substr(pos + 1, endPos - pos - 1);
            if (hasAbba(hypernet)) {
                return true;
            }
            pos = endPos + 1;
        } else {
            break;
        }
    }
    return false;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile) {
        std::cerr << "Error opening input file.\n";
        return 1;
    }

    int tlsCount = 0;
    std::string line;
    while (std::getline(inputFile, line)) {
        if (hasAbba(line) && !hasAbbaInHypernet(line)) {
            ++tlsCount;
        }
    }

    std::cout << "Number of IPs that support TLS: " << tlsCount << std::endl;

    return 0;
}