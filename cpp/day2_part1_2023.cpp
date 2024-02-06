#include <iostream>
#include <fstream>
#include <regex>
#include <string>
#include <vector>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::regex gameRegex("Game (\\d+): (.+)");
    std::regex cubeRegex("(\\d+) (red|green|blue)");
    int totalSum = 0;

    std::string line;
    while (std::getline(file, line)) {
        std::smatch matches;
        if (std::regex_match(line, matches, gameRegex)) {
            int gameId = std::stoi(matches[1]);
            std::string rounds = matches[2];
            bool isValid = true;

            std::vector<std::string> roundTokens;
            size_t pos = 0;
            while ((pos = rounds.find(";")) != std::string::npos) {
                roundTokens.push_back(rounds.substr(0, pos));
                rounds.erase(0, pos + 1);
            }
            roundTokens.push_back(rounds);

            for (const auto& round : roundTokens) {
                std::vector<std::string> cubes;
                std::sregex_iterator it(round.begin(), round.end(), cubeRegex);
                std::sregex_iterator end;
                int red = 0, green = 0, blue = 0;

                while (it != end) {
                    std::smatch match = *it;
                    int count = std::stoi(match[1]);
                    std::string color = match[2];

                    if (color == "red") {
                        red += count;
                    } else if (color == "green") {
                        green += count;
                    } else if (color == "blue") {
                        blue += count;
                    }

                    if (red > 12 || green > 13 || blue > 14) {
                        isValid = false;
                        break;
                    }

                    ++it;
                }

                if (!isValid) {
                    break;
                }
            }

            if (isValid) {
                totalSum += gameId;
            }
        }
    }

    file.close();

    std::cout << totalSum << std::endl;

    return 0;
}