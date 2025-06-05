
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <algorithm>
#include <stdexcept>

struct Gate {
    std::string input1;
    std::string op;
    std::string input2;
    std::string output;
};

long long solve() {
    std::ifstream file("input.txt");
    std::string line;
    std::vector<std::string> lines;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    file.close();

    std::unordered_map<std::string, int> wires;
    std::vector<Gate> gates;
    bool parsing_wires = true;

    std::regex wire_pattern(R"s((\w+):\s*([01]))s");
    std::regex gate_pattern(R"s((\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+))s");

    for (const std::string& current_line : lines) {
        if (current_line.empty()) {
            parsing_wires = false;
            continue;
        }

        std::smatch match;
        if (parsing_wires) {
            if (std::regex_match(current_line, match, wire_pattern)) {
                wires[match[1].str()] = std::stoi(match[2].str());
            }
        } else {
            if (std::regex_match(current_line, match, gate_pattern)) {
                gates.push_back({match[1].str(), match[2].str(), match[3].str(), match[4].str()});
            }
        }
    }

    std::vector<Gate> remaining_gates = gates;
    while (!remaining_gates.empty()) {
        bool progress = false;
        std::vector<Gate> new_remaining_gates;

        for (const Gate& gate : remaining_gates) {
            auto it1 = wires.find(gate.input1);
            auto it2 = wires.find(gate.input2);

            if (it1 != wires.end() && it2 != wires.end()) {
                int val1 = it1->second;
                int val2 = it2->second;
                int result;

                if (gate.op == "AND") {
                    result = val1 & val2;
                } else if (gate.op == "OR") {
                    result = val1 | val2;
                } else if (gate.op == "XOR") {
                    result = val1 ^ val2;
                } else {
                    throw std::runtime_error("Unknown operator: " + gate.op);
                }
                wires[gate.output] = result;
                progress = true;
            } else {
                new_remaining_gates.push_back(gate);
            }
        }

        if (!progress) {
            throw std::runtime_error("Cannot evaluate remaining gates due to missing inputs or cyclic dependencies.");
        }
        remaining_gates = new_remaining_gates;
    }

    std::vector<std::pair<int, int>> z_wires_indexed;
    std::regex z_wire_pattern(R"s(z(\d+))s");

    for (const auto& pair : wires) {
        std::smatch match;
        if (std::regex_match(pair.first, match, z_wire_pattern)) {
            z_wires_indexed.push_back({std::stoi(match[1].str()), pair.second});
        }
    }

    if (z_wires_indexed.empty()) {
        throw std::runtime_error("No wires starting with 'z' found.");
    }

    std::sort(z_wires_indexed.begin(), z_wires_indexed.end(), [](const auto& a, const auto& b) {
        return a.first > b.first;
    });

    std::string binary_string = "";
    binary_string.reserve(z_wires_indexed.size()); // Optimization
    for (const auto& pair : z_wires_indexed) {
        binary_string += std::to_string(pair.second);
    }

    return std::stoull(binary_string, nullptr, 2);
}

int main() {
    try {
        long long result = solve();
        std::cout << result << std::endl;
    } catch (const std::runtime_error& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    return 0;
}
