
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <numeric>

long long gcd(long long a, long long b) {
    while (b) {
        a %= b;
        std::swap(a, b);
    }
    return a;
}

long long lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return (a / gcd(a, b)) * b;
}

std::pair<std::string, std::pair<std::string, std::string>> parse_line(const std::string& line) {
    size_t eqPos = line.find(" = ");
    std::string head = line.substr(0, eqPos);

    size_t childrenStart = line.find('(') + 1;
    size_t childrenEnd = line.find(')');
    std::string childrenStr = line.substr(childrenStart, childrenEnd - childrenStart);

    size_t commaPos = childrenStr.find(", ");
    std::string left = childrenStr.substr(0, commaPos);
    std::string right = childrenStr.substr(commaPos + 2);

    return {head, {left, right}};
}

std::pair<std::string, std::map<std::string, std::pair<std::string, std::string>>> parse_input(const std::vector<std::string>& input) {
    std::string instructions = input[0];

    std::map<std::string, std::pair<std::string, std::string>> nodes;
    for (size_t i = 2; i < input.size(); ++i) {
        auto [head, children] = parse_line(input[i]);
        nodes[head] = children;
    }
    return {instructions, nodes};
}

long long solve(const std::vector<std::string>& inputData) {
    auto [instructions, nodes] = parse_input(inputData);

    std::vector<std::string> starts;
    for (const auto& pair : nodes) {
        if (pair.first.back() == 'A') {
            starts.push_back(pair.first);
        }
    }

    std::vector<long long> steps(starts.size());
    size_t instructionsLength = instructions.length();

    for (size_t i = 0; i < starts.size(); ++i) {
        std::string element = starts[i];
        while (element.back() != 'Z') {
            char instruction = instructions[steps[i] % instructionsLength];
            if (instruction == 'L') {
                element = nodes[element].first;
            } else {
                element = nodes[element].second;
            }
            steps[i]++;
        }
    }

    long long resultLcm = steps[0];
    for (size_t i = 1; i < steps.size(); ++i) {
        resultLcm = lcm(resultLcm, steps[i]);
    }

    return resultLcm;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> inputData;
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }
    std::string line;
    while (std::getline(file, line)) {
        inputData.push_back(line);
    }
    file.close();

    std::cout << solve(inputData) << std::endl;

    return 0;
}
