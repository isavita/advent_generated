
#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>

char getLeastCommonChar(std::unordered_map<char, int>& count) {
    char minChar;
    int minCount = INT_MAX;

    for (auto& pair : count) {
        if (pair.second < minCount) {
            minCount = pair.second;
            minChar = pair.first;
        }
    }

    return minChar;
}

std::string getOriginalMessage(std::vector<std::string>& messages) {
    if (messages.size() == 0) {
        return "";
    }

    int messageLength = messages[0].size();
    std::vector<std::unordered_map<char, int>> count(messageLength);

    for (int i = 0; i < messageLength; i++) {
        count[i] = std::unordered_map<char, int>();
    }

    for (auto& message : messages) {
        for (int j = 0; j < message.length(); j++) {
            count[j][message[j]]++;
        }
    }

    std::string originalMessage = "";
    for (auto& charCount : count) {
        originalMessage += getLeastCommonChar(charCount);
    }

    return originalMessage;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> messages;
    std::string line;
    while (std::getline(file, line)) {
        messages.push_back(line);
    }

    file.close();

    std::string originalMessage = getOriginalMessage(messages);
    std::cout << originalMessage << std::endl;

    return 0;
}
