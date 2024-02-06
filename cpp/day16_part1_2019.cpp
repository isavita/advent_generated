
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cmath>

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

std::vector<int> applyFFT(std::vector<int>& input) {
    std::vector<int> basePattern = {0, 1, 0, -1};
    std::vector<int> output(input.size(), 0);
    for (int i = 0; i < input.size(); i++) {
        int sum = 0;
        for (int j = 0; j < input.size(); j++) {
            int patternValue = basePattern[((j + 1) / (i + 1)) % basePattern.size()];
            sum += input[j] * patternValue;
        }
        output[i] = abs(sum % 10);
    }
    return output;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string input;
    std::getline(file, input);

    // Convert input string to vector of ints
    std::vector<int> digits;
    for (char c : input) {
        digits.push_back(c - '0');
    }

    // Apply FFT algorithm for 100 phases
    for (int phase = 0; phase < 100; phase++) {
        digits = applyFFT(digits);
    }

    // Output the first eight digits
    for (int i = 0; i < 8; i++) {
        std::cout << digits[i];
    }
    std::cout << std::endl;

    return 0;
}
