#include <iostream>
#include <fstream>
#include <vector>
#include <string>

int execute(std::vector<int>& memory) {
    for (int i = 0; i < memory.size(); i += 4) {
        switch (memory[i]) {
            case 1:
                memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]];
                break;
            case 2:
                memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]];
                break;
            case 99:
                return memory[0];
        }
    }
    return memory[0];
}

int main() {
    std::ifstream input("input.txt");
    std::string line;
    std::getline(input, line);
    input.close();

    std::vector<int> original;
    size_t pos = 0;
    std::string token;
    while ((pos = line.find(',')) != std::string::npos) {
        token = line.substr(0, pos);
        original.push_back(std::stoi(token));
        line.erase(0, pos + 1);
    }
    original.push_back(std::stoi(line));

    for (int noun = 0; noun <= 99; noun++) {
        for (int verb = 0; verb <= 99; verb++) {
            std::vector<int> memory(original);
            memory[1] = noun;
            memory[2] = verb;
            if (execute(memory) == 19690720) {
                std::cout << 100 * noun + verb << std::endl;
                return 0;
            }
        }
    }

    return 0;
}