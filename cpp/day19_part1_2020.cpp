
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <sstream>
#include <stdexcept> // Required for std::out_of_range

std::unordered_map<std::string, std::string> rules_dict;
std::unordered_map<std::string, std::string> pattern_cache;

// Helper function to trim leading/trailing whitespace
std::string trim(const std::string& str) {
    size_t first = str.find_first_not_of(' ');
    if (std::string::npos == first) {
        return str;
    }
    size_t last = str.find_last_not_of(' ');
    return str.substr(first, (last - first + 1));
}


std::string get_rule_pattern(const std::string& rule_num) {
    if (pattern_cache.count(rule_num)) {
        return pattern_cache[rule_num];
    }

    if (!rules_dict.count(rule_num)) {
         throw std::runtime_error("Rule not found: " + rule_num);
    }
    const std::string& rule = rules_dict.at(rule_num);
    std::string generated_pattern;

    if (rule[0] == '"') {
        generated_pattern = rule.substr(1, 1);
    } else {
        generated_pattern = "(";
        std::stringstream ss_or(rule);
        std::string or_part;
        bool first_or = true;

        while (std::getline(ss_or, or_part, '|')) {
             or_part = trim(or_part); // Trim whitespace around the part
             if (or_part.empty()) continue;

             if (!first_or) {
                 generated_pattern += "|";
             }
             first_or = false;

             std::stringstream ss_and(or_part);
             std::string sub_rule_num;
             while (ss_and >> sub_rule_num) { // Reads space-separated numbers
                 generated_pattern += get_rule_pattern(sub_rule_num);
             }
        }
        generated_pattern += ")";
    }

    pattern_cache[rule_num] = generated_pattern;
    return generated_pattern;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    // std::cin.tie(NULL); // Not strictly needed as we only use cout at the end

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::string line;
    bool reading_rules = true;
    std::vector<std::string> messages;

    while (std::getline(file, line)) {
        if (line.empty()) {
            reading_rules = false;
            continue;
        }
        if (reading_rules) {
            size_t colon_pos = line.find(": ");
            if (colon_pos != std::string::npos) {
                 std::string key = line.substr(0, colon_pos);
                 std::string value = line.substr(colon_pos + 2);
                 rules_dict[key] = value;
            }
        } else {
            messages.push_back(line);
        }
    }
    file.close();

    try {
        std::string pattern_str = "^" + get_rule_pattern("0") + "$";
        std::regex pattern(pattern_str);

        int count = 0;
        for (const std::string& message : messages) {
            if (!message.empty() && std::regex_match(message, pattern)) {
                count++;
            }
        }
        std::cout << count << std::endl;

    } catch (const std::regex_error& e) {
        std::cerr << "Regex error: " << e.what() << " (Code: " << e.code() << ")" << std::endl;
        return 1;
    } catch (const std::exception& e) {
         std::cerr << "Error: " << e.what() << std::endl;
         return 1;
    }


    return 0;
}
