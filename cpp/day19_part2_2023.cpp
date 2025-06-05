
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <array>
#include <regex>
#include <algorithm> // For std::min, std::max

// Rule structure
struct Rule {
    char var = 0; // 'x', 'm', 'a', 's', or 0 for no condition
    char op = 0;  // '<', '>', or 0 for no condition
    int val = 0;  // Value to compare against
    std::string dest; // Destination workflow
};

// Type aliases for clarity
using Range = std::pair<int, int>;
using Constraints = std::array<Range, 4>; // x, m, a, s (indices: 0, 1, 2, 3)

// Global data structures
std::map<std::string, std::vector<Rule>> workflows;
std::map<char, int> variableIndices = {{'x', 0}, {'m', 1}, {'a', 2}, {'s', 3}};
std::map<std::pair<std::string, Constraints>, long long> memo;

// Recursive function to process constraints
long long process(const std::string& workflowName, Constraints constraints) {
    // Check memoization cache
    auto key = std::make_pair(workflowName, constraints);
    if (memo.count(key)) {
        return memo[key];
    }

    // Base cases for workflow destinations
    if (workflowName == "R") {
        return 0; // Rejected
    }
    if (workflowName == "A") {
        // Accepted: calculate combinations
        long long count = 1;
        for (const auto& range : constraints) {
            // If any range is invalid (min > max), it means no possible values
            if (range.first > range.second) {
                return 0;
            }
            count *= (long long)(range.second - range.first + 1);
        }
        return count;
    }

    const auto& currentWorkflowRules = workflows.at(workflowName);
    long long totalAcceptedCount = 0;

    Constraints currentConstraintsForRules = constraints; // Mutable copy for rule propagation

    for (const auto& rule : currentWorkflowRules) {
        if (rule.var == 0) { // No condition, always applies
            totalAcceptedCount += process(rule.dest, currentConstraintsForRules);
            break; // This rule is terminal for the current path
        }

        // Conditioned rule
        int varIdx = variableIndices.at(rule.var);
        Range varRange = currentConstraintsForRules[varIdx];

        Range trueRange;
        Range falseRange;

        if (rule.op == '>') {
            trueRange = {std::max(varRange.first, rule.val + 1), varRange.second};
            falseRange = {varRange.first, std::min(varRange.second, rule.val)};
        } else { // '<'
            trueRange = {varRange.first, std::min(varRange.second, rule.val - 1)};
            falseRange = {std::max(varRange.first, rule.val), varRange.second};
        }

        // Process the branch where the condition is true
        if (trueRange.first <= trueRange.second) {
            Constraints newConstraints = currentConstraintsForRules;
            newConstraints[varIdx] = trueRange;
            totalAcceptedCount += process(rule.dest, newConstraints);
        }

        // Update currentConstraintsForRules for the false branch (remaining values pass to next rule)
        if (falseRange.first <= falseRange.second) {
            currentConstraintsForRules[varIdx] = falseRange;
            // Continue loop to next rule with updated constraints
        } else {
            // No valid values left for the false branch, so no further rules in this workflow can apply
            break;
        }
    }

    memo[key] = totalAcceptedCount;
    return totalAcceptedCount;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::string line;
    std::regex workflowRegex(R"((.+)\{(.+)\})");
    std::regex conditionRegex(R"(([xmas])([<>])(\d+))");

    // Parse workflows
    while (std::getline(inputFile, line) && !line.empty()) {
        std::smatch workflowMatch;
        if (std::regex_match(line, workflowMatch, workflowRegex)) {
            std::string name = workflowMatch.str(1);
            std::string rulesStr = workflowMatch.str(2);

            size_t start = 0;
            while (start < rulesStr.length()) {
                size_t commaPos = rulesStr.find(',', start);
                std::string rulePart;
                if (commaPos == std::string::npos) {
                    rulePart = rulesStr.substr(start);
                    start = rulesStr.length(); // End loop
                } else {
                    rulePart = rulesStr.substr(start, commaPos - start);
                    start = commaPos + 1;
                }

                Rule rule;
                size_t colonPos = rulePart.find(':');
                if (colonPos != std::string::npos) {
                    std::string conditionStr = rulePart.substr(0, colonPos);
                    rule.dest = rulePart.substr(colonPos + 1);

                    std::smatch condMatch;
                    if (std::regex_match(conditionStr, condMatch, conditionRegex)) {
                        rule.var = condMatch.str(1)[0];
                        rule.op = condMatch.str(2)[0];
                        rule.val = std::stoi(condMatch.str(3));
                    } else {
                        std::cerr << "Error parsing condition: " << conditionStr << std::endl;
                        return 1;
                    }
                } else {
                    rule.var = 0; // No condition
                    rule.op = 0;
                    rule.val = 0;
                    rule.dest = rulePart;
                }
                workflows[name].push_back(rule);
            }
        }
    }

    // Initial constraints: all variables in [1,4000]
    Constraints initialConstraints = {{{1,4000},{1,4000},{1,4000},{1,4000}}};

    long long total = process("in", initialConstraints);

    std::cout << total << std::endl;

    return 0;
}
