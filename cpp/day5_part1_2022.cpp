#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

std::string move(std::vector<std::vector<char> > st, std::vector<std::string> steps) {
    std::vector<std::vector<char> > stacks(st.size());
    for (int i = 0; i < st.size(); i++) {
        stacks[i] = std::vector<char>(st[i].size());
        for (int j = 0; j < st[i].size(); j++) {
            stacks[i][j] = st[i][st[i].size() - j - 1];
        }
    }

    for (std::string step : steps) {
        int n, from, to;
        std::sscanf(step.c_str(), "move %d from %d to %d", &n, &from, &to);
        from--;
        to--;
        for (int i = 0; i < n; i++) {
            stacks[to].push_back(stacks[from][stacks[from].size() - 1]);
            stacks[from].pop_back();
        }
    }

    std::string result;
    for (int i = 0; i < stacks.size(); i++) {
        result += stacks[i][stacks[i].size() - 1];
    }
    return result;
}

std::string readAll(std::string path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file");
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
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
    
    std::vector<std::string> input_lines;
    pos = 0;
    while ((pos = s[0].find("\n")) != std::string::npos) {
        token = s[0].substr(0, pos);
        input_lines.push_back(token);
        s[0].erase(0, pos + 1);
    }
    input_lines.push_back(s[0]);

    std::vector<std::vector<char> > stacks((input_lines[0].size() + 1) / 4);
    for (std::string line : input_lines) {
        for (int i = 0; i < line.size(); i++) {
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