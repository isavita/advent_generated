#include <iostream>
#include <fstream>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "File reading error" << std::endl;
        return 1;
    }

    int score = 0;
    int depth = 0;
    bool inGarbage = false;
    bool cancelNext = false;

    std::string line;
    while (std::getline(file, line)) {
        for (char ch : line) {
            if (cancelNext) {
                cancelNext = false;
                continue;
            }

            if (inGarbage) {
                if (ch == '!') {
                    cancelNext = true;
                } else if (ch == '>') {
                    inGarbage = false;
                }
            } else {
                switch (ch) {
                    case '{':
                        depth++;
                        break;
                    case '}':
                        score += depth;
                        depth--;
                        break;
                    case '<':
                        inGarbage = true;
                        break;
                }
            }
        }
    }

    std::cout << score << std::endl;

    return 0;
}