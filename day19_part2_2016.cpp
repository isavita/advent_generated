
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

struct LLNode {
    int elfNum;
    int presents;
    LLNode* next;
};

int elephant(std::string input) {
    int startingElves = std::stoi(input);
    LLNode* root = new LLNode{1, 1, nullptr};
    LLNode* iter = root;

    for (int i = 2; i <= startingElves; i++) {
        iter->next = new LLNode{i, 1, nullptr};
        iter = iter->next;
    }
    iter->next = root;

    bool isOddLength = startingElves % 2 == 1;
    LLNode* beforeAcross = root;
    for (int i = 0; i < startingElves / 2 - 1; i++) {
        beforeAcross = beforeAcross->next;
    }

    while (root->next != root) {
        root->presents += beforeAcross->next->presents;
        beforeAcross->next = beforeAcross->next->next;

        if (isOddLength) {
            beforeAcross = beforeAcross->next;
        }
        isOddLength = !isOddLength;
        root = root->next;
    }

    return root->elfNum;
}

std::string readFile(std::string pathFromCaller) {
    std::ifstream file(pathFromCaller);
    std::string content;
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            content += line;
        }
        file.close();
    }
    return content;
}

int main() {
    std::string input = readFile("input.txt");
    int ans = elephant(input);
    std::cout << ans << std::endl;
    return 0;
}
