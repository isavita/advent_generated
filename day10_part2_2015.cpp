
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

std::string readInput(std::string filename) {
    std::ifstream file(filename);
    std::string sequence;
    if (file.is_open()) {
        std::getline(file, sequence);
    }
    file.close();
    return sequence;
}

std::string nextSequence(std::string sequence) {
    std::stringstream result;
    for (size_t i = 0; i < sequence.length();) {
        int count = 1;
        char digit = sequence[i];
        for (size_t j = i + 1; j < sequence.length() && sequence[j] == digit; j++) {
            count++;
        }
        result << count << digit;
        i += count;
    }
    return result.str();
}

std::string lookAndSay(std::string sequence, int iterations) {
    for (int i = 0; i < iterations; i++) {
        sequence = nextSequence(sequence);
    }
    return sequence;
}

int main() {
    std::string initialSequence = readInput("input.txt");
    std::string result = lookAndSay(initialSequence, 50);
    std::cout << result.length() << std::endl;
    return 0;
}
