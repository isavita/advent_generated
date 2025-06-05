
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

std::vector<std::string> split(const std::string& s, const std::string& delimiter) {
    std::vector<std::string> tokens;
    size_t prev = 0;
    size_t pos = 0;
    while ((pos = s.find(delimiter, prev)) != std::string::npos) {
        tokens.push_back(s.substr(prev, pos - prev));
        prev = pos + delimiter.length();
    }
    tokens.push_back(s.substr(prev));
    return tokens;
}

struct Rule {
    std::vector<std::string> resolved;
    std::vector<std::vector<int>> options;
};

std::vector<std::string> fill_in_graph(std::map<int, Rule>& graph, int entry) {
    if (!graph[entry].resolved.empty()) {
        return graph[entry].resolved;
    }

    std::vector<std::string> current_resolved;

    for (const auto& option : graph[entry].options) {
        std::vector<std::string> resolved_for_option = {""};
        for (int entry_point : option) {
            std::vector<std::string> nested_resolve_vals = fill_in_graph(graph, entry_point);
            std::vector<std::string> new_resolved_for_option;
            for (const std::string& next_piece : nested_resolve_vals) {
                for (const std::string& prev : resolved_for_option) {
                    new_resolved_for_option.push_back(prev + next_piece);
                }
            }
            resolved_for_option = new_resolved_for_option;
        }
        current_resolved.insert(current_resolved.end(), resolved_for_option.begin(), resolved_for_option.end());
    }

    graph[entry].resolved = current_resolved;
    return graph[entry].resolved;
}

std::pair<std::map<int, Rule>, std::vector<std::string>> parse_input(const std::string& input_data) {
    std::vector<std::string> parts = split(input_data, "\n\n");

    std::map<int, Rule> rules;
    std::istringstream rules_stream(parts[0]);
    std::string line;
    std::regex char_rule_regex("(\\d+): \"(\\w)\"");
    std::regex option_rule_regex("(\\d+): (.+)");

    while (std::getline(rules_stream, line)) {
        std::smatch matches;
        if (std::regex_match(line, matches, char_rule_regex)) {
            int num = std::stoi(matches[1].str());
            rules[num] = Rule();
            rules[num].resolved.push_back(matches[2].str());
        } else if (std::regex_match(line, matches, option_rule_regex)) {
            int key = std::stoi(matches[1].str());
            rules[key] = Rule();
            std::string rule_nums_str = matches[2].str();
            std::vector<std::string> options_str = split(rule_nums_str, " | ");
            for (const std::string& option_str : options_str) {
                std::vector<int> nums;
                std::istringstream iss_option(option_str);
                std::string num_str;
                while (iss_option >> num_str) {
                    nums.push_back(std::stoi(num_str));
                }
                rules[key].options.push_back(nums);
            }
        }
    }

    std::vector<std::string> messages;
    std::istringstream messages_stream(parts[1]);
    while (std::getline(messages_stream, line)) {
        messages.push_back(line);
    }
    if (!messages.empty() && messages.back().empty()) {
        messages.pop_back();
    }

    return {rules, messages};
}

int solve(const std::string& input_data) {
    auto [graph, messages] = parse_input(input_data);

    fill_in_graph(graph, 42);
    fill_in_graph(graph, 31);

    std::string part42_str = "(";
    for (size_t i = 0; i < graph[42].resolved.size(); ++i) {
        part42_str += graph[42].resolved[i];
        if (i < graph[42].resolved.size() - 1) {
            part42_str += "|";
        }
    }
    part42_str += ")";

    std::string part31_str = "(";
    for (size_t i = 0; i < graph[31].resolved.size(); ++i) {
        part31_str += graph[31].resolved[i];
        if (i < graph[31].resolved.size() - 1) {
            part31_str += "|";
        }
    }
    part31_str += ")";

    std::string rule8_string = "(" + part42_str + ")+";

    std::vector<std::regex> compiled_patterns(10);
    for (int i = 1; i <= 9; ++i) {
        std::string pattern_str = "^" + rule8_string;
        for (int k = 0; k < i; ++k) {
            pattern_str += part42_str;
        }
        for (int k = 0; k < i; ++k) {
            pattern_str += part31_str;
        }
        pattern_str += "$";
        compiled_patterns[i] = std::regex(pattern_str);
    }

    int match_rule_zero = 0;
    for (const std::string& m : messages) {
        for (int i = 1; i <= 9; ++i) {
            if (std::regex_match(m, compiled_patterns[i])) {
                match_rule_zero++;
                break;
            }
        }
    }

    return match_rule_zero;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string input_data = buffer.str();
    file.close();

    std::cout << solve(input_data) << std::endl;

    return 0;
}
