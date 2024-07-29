#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <unordered_map>

void spin(std::string &programs, int x) {
    int n = programs.size();
    std::rotate(programs.begin(), programs.begin() + n - x, programs.end());
}

void exchange(std::string &programs, int a, int b) {
    std::swap(programs[a], programs[b]);
}

void partner(std::string &programs, char a, char b) {
    int posA = programs.find(a);
    int posB = programs.find(b);
    std::swap(programs[posA], programs[posB]);
}

void performDance(std::string &programs, const std::vector<std::string> &moves) {
    for (const auto &move : moves) {
        if (move[0] == 's') {
            spin(programs, std::stoi(move.substr(1)));
        } else if (move[0] == 'x') {
            int a = std::stoi(move.substr(1, move.find('/') - 1));
            int b = std::stoi(move.substr(move.find('/') + 1));
            exchange(programs, a, b);
        } else if (move[0] == 'p') {
            partner(programs, move[1], move[3]);
        }
    }
}

std::vector<std::string> parseMoves(const std::string &input) {
    std::vector<std::string> moves;
    size_t start = 0, end;
    while ((end = input.find(',', start)) != std::string::npos) {
        moves.push_back(input.substr(start, end - start));
        start = end + 1;
    }
    moves.push_back(input.substr(start));
    return moves;
}

std::string danceWithCycles(std::string programs, const std::vector<std::string> &moves, int repetitions) {
    std::unordered_map<std::string, int> seen;
    int cycleStart = 0, cycleLength = 0;

    for (int i = 0; i < repetitions; ++i) {
        if (seen.count(programs)) {
            cycleStart = seen[programs];
            cycleLength = i - cycleStart;
            break;
        }
        seen[programs] = i;
        performDance(programs, moves);
    }

    if (cycleLength) {
        repetitions = (repetitions - cycleStart) % cycleLength + cycleStart;
        for (const auto &entry : seen) {
            if (entry.second == repetitions) {
                return entry.first;
            }
        }
    }
    return programs;
}

int main() {
    std::ifstream infile("input.txt");
    std::string input;
    std::getline(infile, input);
    
    std::vector<std::string> moves = parseMoves(input);
    std::string programs = "abcdefghijklmnop";

    programs = danceWithCycles(programs, moves, 1000000000);
    std::cout << programs << std::endl;

    return 0;
}