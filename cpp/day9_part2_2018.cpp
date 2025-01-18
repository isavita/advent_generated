
#include <iostream>
#include <fstream>
#include <vector>
#include <list>

int main() {
    std::ifstream file("input.txt");
    int players, lastMarble;
    std::string line;
    std::getline(file, line);
    sscanf(line.c_str(), "%d players; last marble is worth %d points", &players, &lastMarble);
    lastMarble *= 100;

    std::vector<long long> scores(players, 0);
    std::list<int> circle;
    circle.push_back(0);
    auto current = circle.begin();

    for (int marble = 1; marble <= lastMarble; ++marble) {
        if (marble % 23 == 0) {
            int player = marble % players;
            for (int i = 0; i < 7; ++i) {
                if (current == circle.begin()) {
                    current = circle.end();
                }
                --current;
            }
            scores[player] += marble + *current;
            current = circle.erase(current);
            if (current == circle.end()) {
                current = circle.begin();
            }
        } else {
            ++current;
            if (current == circle.end()) {
                current = circle.begin();
            }
            ++current;
            current = circle.insert(current, marble);
        }
    }

    long long maxScore = 0;
    for (long long score : scores) {
        if (score > maxScore) {
            maxScore = score;
        }
    }
    std::cout << maxScore << std::endl;

    return 0;
}
