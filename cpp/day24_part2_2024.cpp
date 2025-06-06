
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <sstream>
#include <algorithm>
#include <utility>

struct Gate {
    std::string a, op, b;
};

struct GateWithOutput {
    Gate gate;
    std::string output;
};

std::vector<GateWithOutput> parse(const std::string& input) {
    std::vector<GateWithOutput> gates;
    size_t pos = input.find("\n\n");
    if (pos == std::string::npos) return gates;

    std::stringstream ss(input.substr(pos + 2));
    std::string line;
    while (std::getline(ss, line)) {
        if (line.empty()) continue;
        std::stringstream line_ss(line);
        std::string a, op, b, arrow, output;
        line_ss >> a >> op >> b >> arrow >> output;
        if (arrow == "->" && !output.empty()) {
            gates.push_back({{a, op, b}, output});
        }
    }
    return gates;
}

std::string getReverseLookupKey(std::string a, const std::string& op, std::string b) {
    if (a > b) std::swap(a, b);
    return a + "_" + op + "_" + b;
}

std::pair<std::unordered_map<std::string, Gate>, std::unordered_map<std::string, std::string>>
createLookups(const std::vector<GateWithOutput>& gates) {
    std::unordered_map<std::string, Gate> lookup;
    std::unordered_map<std::string, std::string> reverseLookup;
    for (const auto& g : gates) {
        lookup[g.output] = g.gate;
        reverseLookup[getReverseLookupKey(g.gate.a, g.gate.op, g.gate.b)] = g.output;
    }
    return {lookup, reverseLookup};
}

void swap_wires(std::vector<std::pair<std::string, std::string>>& pairs,
                std::vector<GateWithOutput>& gates,
                const std::string& a,
                const std::string& b) {
    pairs.emplace_back(a, b);
    for (auto& g : gates) {
        if (g.output == a) g.output = b;
        else if (g.output == b) g.output = a;
    }
}

std::string find_in_map(const std::unordered_map<std::string, std::string>& map, const std::string& key) {
    auto it = map.find(key);
    return (it != map.end()) ? it->second : "";
}

std::string pad_num(int i) {
    std::string s = std::to_string(i);
    return (s.length() < 2) ? "0" + s : s;
}

std::string solution(std::vector<GateWithOutput>& gates) {
    std::vector<std::pair<std::string, std::string>> pairs;
    int numZ = 0;
    for (const auto& g : gates) {
        if (!g.output.empty() && g.output[0] == 'z') numZ++;
    }

    while (pairs.size() < 4) {
        auto [lookup, reverseLookup] = createLookups(gates);
        std::string carry_in;

        for (int i = 0; i < numZ; ++i) {
            std::string xi = "x" + pad_num(i);
            std::string yi = "y" + pad_num(i);
            std::string zi = "z" + pad_num(i);
            
            bool swapped = false;
            std::string found_adder;
            std::string carry_out = carry_in;

            if (i == 0) {
                found_adder = find_in_map(reverseLookup, getReverseLookupKey(xi, "XOR", yi));
                carry_out = find_in_map(reverseLookup, getReverseLookupKey(xi, "AND", yi));
            } else {
                std::string bit = find_in_map(reverseLookup, getReverseLookupKey(xi, "XOR", yi));
                if (!bit.empty() && !carry_in.empty()) {
                    found_adder = find_in_map(reverseLookup, getReverseLookupKey(bit, "XOR", carry_in));
                    if (!found_adder.empty()) {
                        std::string c1 = find_in_map(reverseLookup, getReverseLookupKey(xi, "AND", yi));
                        std::string c2 = find_in_map(reverseLookup, getReverseLookupKey(bit, "AND", carry_in));
                        carry_out = (!c1.empty() && !c2.empty()) ? find_in_map(reverseLookup, getReverseLookupKey(c1, "OR", c2)) : "";
                    }
                }
            }
            
            if (found_adder.empty()) {
                if (!carry_in.empty() && lookup.count(zi)) {
                    const auto& gate = lookup.at(zi);
                    std::string bit = find_in_map(reverseLookup, getReverseLookupKey(xi, "XOR", yi));
                    if (!bit.empty()) {
                        if (!find_in_map(reverseLookup, getReverseLookupKey(gate.a, "XOR", carry_in)).empty()) {
                            swap_wires(pairs, gates, bit, gate.a);
                            swapped = true;
                        } else if (!find_in_map(reverseLookup, getReverseLookupKey(gate.b, "XOR", carry_in)).empty()) {
                            swap_wires(pairs, gates, bit, gate.b);
                            swapped = true;
                        }
                    }
                }
            } else if (found_adder != zi) {
                swap_wires(pairs, gates, found_adder, zi);
                swapped = true;
            }

            if (swapped) break;
            carry_in = carry_out;
        }
    }

    std::vector<std::string> result_parts;
    for (const auto& p : pairs) {
        result_parts.push_back(p.first);
        result_parts.push_back(p.second);
    }
    std::sort(result_parts.begin(), result_parts.end());

    std::stringstream result_ss;
    for (size_t i = 0; i < result_parts.size(); ++i) {
        result_ss << result_parts[i] << (i == result_parts.size() - 1 ? "" : ",");
    }
    return result_ss.str();
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file) return 1;

    std::stringstream buffer;
    buffer << file.rdbuf();
    
    auto gates = parse(buffer.str());
    if (gates.empty()) return 1;

    std::cout << solution(gates) << std::endl;

    return 0;
}
