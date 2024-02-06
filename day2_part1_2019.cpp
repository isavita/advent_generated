
#include <iostream>
#include <fstream>
#include <vector>

int executeProgram(std::vector<int>& data);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> inputData;
    std::string line;
    while (std::getline(file, line)) {
        size_t pos = 0;
        std::string token;
        while ((pos = line.find(',')) != std::string::npos) {
            token = line.substr(0, pos);
            int num = std::stoi(token);
            inputData.push_back(num);
            line.erase(0, pos + 1);
        }
        int num = std::stoi(line);
        inputData.push_back(num);
    }
    file.close();

    inputData[1] = 12;
    inputData[2] = 2;

    int result = executeProgram(inputData);

    std::cout << result << std::endl;

    return 0;
}

int executeProgram(std::vector<int>& data) {
    for (int i = 0; i < data.size() - 3; i += 4) {
        int pos1 = data[i + 1];
        int pos2 = data[i + 2];
        int pos3 = data[i + 3];
        switch (data[i]) {
            case 1:
                data[pos3] = data[pos1] + data[pos2];
                break;
            case 2:
                data[pos3] = data[pos1] * data[pos2];
                break;
            case 99:
                return data[0];
            default:
                throw std::invalid_argument("Invalid opcode");
        }
    }

    return data[0];
}
