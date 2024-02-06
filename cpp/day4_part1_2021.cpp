
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>

struct BingoBoard {
    std::vector<std::vector<int> > numbers;
    std::vector<std::vector<bool> > marked;

    void mark(int number) {
        for (int i = 0; i < numbers.size(); i++) {
            for (int j = 0; j < numbers[i].size(); j++) {
                if (numbers[i][j] == number) {
                    marked[i][j] = true;
                }
            }
        }
    }

    bool hasWon() {
        for (int i = 0; i < marked.size(); i++) {
            if (isRowMarked(marked[i]) || isColumnMarked(marked, i)) {
                return true;
            }
        }
        return false;
    }

    int unmarkedSum() {
        int sum = 0;
        for (int i = 0; i < numbers.size(); i++) {
            for (int j = 0; j < numbers[i].size(); j++) {
                if (!marked[i][j]) {
                    sum += numbers[i][j];
                }
            }
        }
        return sum;
    }

    bool isRowMarked(std::vector<bool>& row) {
        for (bool marked : row) {
            if (!marked) {
                return false;
            }
        }
        return true;
    }

    bool isColumnMarked(std::vector<std::vector<bool> >& marked, int column) {
        for (auto& row : marked) {
            if (!row[column]) {
                return false;
            }
        }
        return true;
    }
};

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string line;
    std::getline(file, line);
    std::vector<std::string> numbers;
    std::istringstream iss(line);
    std::string number;
    while (std::getline(iss, number, ',')) {
        numbers.push_back(number);
    }

    std::vector<BingoBoard> boards;
    while (std::getline(file, line)) {
        BingoBoard board;
        board.numbers.resize(5, std::vector<int>(5));
        board.marked.resize(5, std::vector<bool>(5));
        for (int i = 0; i < 5; i++) {
            std::getline(file, line);
            std::istringstream iss2(line);
            for (int j = 0; j < 5; j++) {
                iss2 >> board.numbers[i][j];
            }
        }
        boards.push_back(board);
    }

    BingoBoard* winningBoard = nullptr;
    int winningNumber = 0;
    for (const auto& num : numbers) {
        int n = std::stoi(num);
        for (auto& board : boards) {
            board.mark(n);
            if (board.hasWon()) {
                winningBoard = &board;
                winningNumber = n;
                break;
            }
        }
        if (winningBoard != nullptr) {
            break;
        }
    }

    if (winningBoard != nullptr) {
        std::cout << winningBoard->unmarkedSum() * winningNumber << std::endl;
    }

    return 0;
}
