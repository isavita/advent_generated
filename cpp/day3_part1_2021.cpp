
#include <iostream>
#include <fstream>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    int counts[12][2] = {0};
    std::string num;
    while (std::getline(file, num)) {
        for (size_t i = 0; i < num.size(); i++) {
            counts[i][num[i] - '0']++;
        }
    }

    int gammaRate = 0;
    int epsilonRate = 0;
    for (int i = 0; i < 12; i++) {
        if (counts[i][0] > counts[i][1]) {
            gammaRate |= 1 << (12 - i - 1);
        } else {
            epsilonRate |= 1 << (12 - i - 1);
        }
    }

    std::cout << gammaRate * epsilonRate << std::endl;

    return 0;
}
