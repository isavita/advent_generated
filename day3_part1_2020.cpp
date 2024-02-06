#include <iostream>
#include <fstream>
#include <vector>

int countTrees(std::vector<std::string> forest, int right, int down) {
    int trees = 0;
    int x = 0;
    int width = forest[0].length();

    for (int y = 0; y < forest.size(); y += down) {
        if (forest[y][x % width] == '#') {
            trees++;
        }
        x += right;
    }

    return trees;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> forest;
    std::string line;
    while (std::getline(file, line)) {
        forest.push_back(line);
    }

    int trees = countTrees(forest, 3, 1);
    std::cout << trees << std::endl;

    return 0;
}