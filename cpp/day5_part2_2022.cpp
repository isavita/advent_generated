
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

std::string move(std::vector<std::vector<char> > st, std::vector<std::string> steps) {
    std::vector<std::vector<char> > stacks(st.size());
    for (int i = 0; i < st.size(); i++) {
        stacks[i].resize(st[i].size());
        std::reverse_copy(st[i].begin(), st[i].end(), stacks[i].begin());
    }

    for (const auto& step : steps) {
        int n, from, to;
        sscanf(step.c_str(), "move %d from %d to %d", &n, &from, &to);
        from--;
        to--;
        stacks[to].insert(stacks[to].end(), stacks[from].end() - n, stacks[from].end());
        stacks[from].resize(stacks[from].size() - n);
    }

    std::string result;
    for (int i = 0; i < stacks.size(); i++) {
        result += stacks[i].back();
    }
    return result;
}

std::string readAll(std::string path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }
    std::string content((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();
    return content;
}

int main() {
    std::string input = readAll("input.txt");
    std::vector<std::string> s;
    size_t pos = 0;
    std::string token;
    while ((pos = input.find("\n\n")) != std::string::npos) {
        token = input.substr(0, pos);
        s.push_back(token);
        input.erase(0, pos + 2);
    }
    s.push_back(input);

    std::vector<std::string> inputLines;
    pos = 0;
    while ((pos = s[0].find("\n")) != std::string::npos) {
        token = s[0].substr(0, pos);
        inputLines.push_back(token);
        s[0].erase(0, pos + 1);
    }
    inputLines.push_back(s[0]);

    std::vector<std::vector<char> > stacks((inputLines[0].size() + 1) / 4);
    for (const auto& line : inputLines) {
        for (size_t i = 0; i < line.size(); i++) {
            char b = line[i];
            if (b >= 'A' && b <= 'Z') {
                stacks[(i - 1) / 4].push_back(b);
            }
        }
    }

    std::vector<std::string> steps;
    pos = 0;
    while ((pos = s[1].find("\n")) != std::string::npos) {
        token = s[1].substr(0, pos);
        steps.push_back(token);
        s[1].erase(0, pos + 1);
    }
    steps.push_back(s[1]);

    std::cout << move(stacks, steps) << std::endl;
    return 0;
}
