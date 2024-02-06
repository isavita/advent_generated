#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

bool isValidTriangle(int a, int b, int c) {
    return a + b > c && a + c > b && b + c > a;
}

int main() {
    std::ifstream file("input.txt");
    std::vector<std::vector<int> > numbers;

    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            std::vector<int> row;
            std::istringstream iss(line);
            int num;
            while (iss >> num) {
                row.push_back(num);
            }
            numbers.push_back(row);
        }
        file.close();
    } else {
        std::cerr << "Unable to open file" << std::endl;
        return 1;
    }

    int validTriangles = 0;
    for (size_t i = 0; i < numbers[0].size(); i++) {
        for (size_t j = 0; j < numbers.size(); j += 3) {
            if (j + 2 < numbers.size() && isValidTriangle(numbers[j][i], numbers[j + 1][i], numbers[j + 2][i])) {
                validTriangles++;
            }
        }
    }

    std::cout << validTriangles << std::endl;

    return 0;
}