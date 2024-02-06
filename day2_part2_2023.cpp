#include <iostream>
#include <fstream>
#include <regex>
#include <sstream>
#include <string>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::regex gameRegex("Game (\\d+): (.+)");
    std::regex cubeRegex("(\\d+) (red|green|blue)");
    int totalPower = 0;

    std::string line;
    while (std::getline(file, line)) {
        std::smatch matches;
        if (std::regex_search(line, matches, gameRegex)) {
            std::string rounds = matches[2];
            int maxRed = 0, maxGreen = 0, maxBlue = 0;

            std::stringstream ss(rounds);
            std::string round;
            while (std::getline(ss, round, ';')) {
                std::sregex_iterator it(round.begin(), round.end(), cubeRegex);
                std::sregex_iterator end;

                int red = 0, green = 0, blue = 0;
                while (it != end) {
                    int count = std::stoi((*it)[1]);
                    std::string color = (*it)[2].str();

                    if (color == "red") {
                        red += count;
                    } else if (color == "green") {
                        green += count;
                    } else if (color == "blue") {
                        blue += count;
                    }

                    ++it;
                }

                if (red > maxRed) {
                    maxRed = red;
                }
                if (green > maxGreen) {
                    maxGreen = green;
                }
                if (blue > maxBlue) {
                    maxBlue = blue;
                }
            }

            int power = maxRed * maxGreen * maxBlue;
            totalPower += power;
        }
    }

    file.close();

    std::cout << totalPower << std::endl;

    return 0;
}