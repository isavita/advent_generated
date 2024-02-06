#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

class Scrambler {
public:
    Scrambler(std::string pw) : pw(pw) {}

    std::string String() {
        return pw;
    }

    void swapPositions(int x, int y) {
        std::swap(pw[x], pw[y]);
    }

    void swapLetters(char x, char y) {
        swapPositions(pw.find(x), pw.find(y));
    }

    void rotate(int steps) {
        int length = pw.length();
        steps = steps % length;
        if (steps < 0) {
            steps += length;
        }
        std::rotate(pw.begin(), pw.begin() + (length - steps), pw.end());
    }

    void rotateLetter(char x) {
        int index = pw.find(x);
        if (index >= 4) {
            index++;
        }
        rotate(index + 1);
    }

    void derotateLetter(char x) {
        int index = pw.find(x);
        int rot;
        if (index % 2 == 1) {
            rot = -(index + 1) / 2;
        } else if (index != 0) {
            rot = (6 - index) / 2;
        } else {
            rot = -1;
        }
        rotate(rot);
    }

    void reverse(int x, int y) {
        std::reverse(pw.begin() + x, pw.begin() + y + 1);
    }

    void move(int x, int y) {
        char ch = pw[x];
        pw.erase(x, 1);
        pw.insert(y, 1, ch);
    }

    Scrambler* scramble(std::vector<std::string> instructions, int direction) {
        if (direction < 0) {
            std::reverse(instructions.begin(), instructions.end());
        }
        for (const auto& instruction : instructions) {
            std::vector<std::string> line;
            size_t start = 0, end = 0;
            while ((end = instruction.find(" ", start)) != std::string::npos) {
                line.push_back(instruction.substr(start, end - start));
                start = end + 1;
            }
            line.push_back(instruction.substr(start));

            if (line[0] == "swap") {
                if (line[1] == "position") {
                    int x = std::stoi(line[2]);
                    int y = std::stoi(line[5]);
                    swapPositions(x, y);
                } else {
                    swapLetters(line[2][0], line[5][0]);
                }
            } else if (line[0] == "rotate") {
                if (line[1] == "based") {
                    if (direction > 0) {
                        rotateLetter(line[6][0]);
                    } else {
                        derotateLetter(line[6][0]);
                    }
                } else {
                    int steps = std::stoi(line[2]);
                    if (line[1] == "left") {
                        steps = -steps;
                    }
                    if (direction < 0) {
                        steps = -steps;
                    }
                    rotate(steps);
                }
            } else if (line[0] == "reverse") {
                int x = std::stoi(line[2]);
                int y = std::stoi(line[4]);
                reverse(x, y);
            } else if (line[0] == "move") {
                int x = std::stoi(line[2]);
                int y = std::stoi(line[5]);
                if (direction < 0) {
                    std::swap(x, y);
                }
                move(x, y);
            }
        }
        return this;
    }

    Scrambler* unscramble(std::vector<std::string> instructions) {
        return scramble(instructions, -1);
    }

private:
    std::string pw;
};

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        instructions.push_back(line);
    }

    std::string hashed = "fbgdceah";
    Scrambler scrambler(hashed);
    Scrambler* result = scrambler.unscramble(instructions);
    std::cout << result->String() << std::endl;

    return 0;
}