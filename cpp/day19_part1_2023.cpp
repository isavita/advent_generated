
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <sstream>

// Represents a part with its ratings
struct Part {
    int ratings[4]; // 0:x, 1:m, 2:a, 3:s

    int sum() const {
        return ratings[0] + ratings[1] + ratings[2] + ratings[3];
    }
};

// Represents a single rule within a workflow
struct Rule {
    int category_index; // 0:x, 1:m, 2:a, 3:s, -1: default/fallback rule
    char op;            // '<', '>', 0 for default rule
    int value;
    std::string destination;
};

// Represents a workflow with its rules
using Workflow = std::vector<Rule>;

// Maps category characters to indices
int category_to_index(char c) {
    switch (c) {
        case 'x': return 0;
        case 'm': return 1;
        case 'a': return 2;
        case 's': return 3;
        default: return -1; // Should not happen for valid conditions
    }
}

// Evaluates a part against a single rule
bool evaluate_rule(const Part& part, const Rule& rule, std::string& next_workflow) {
    if (rule.category_index == -1) { // Default rule
        next_workflow = rule.destination;
        return true;
    }

    int part_value = part.ratings[rule.category_index];
    bool condition_met = false;

    if (rule.op == '<') {
        condition_met = part_value < rule.value;
    } else if (rule.op == '>') {
        condition_met = part_value > rule.value;
    }

    if (condition_met) {
        next_workflow = rule.destination;
        return true;
    }

    return false; // Condition not met, try next rule
}

// Processes a part through the workflows
bool process_part(const Part& part, const std::unordered_map<std::string, Workflow>& workflows) {
    std::string current_workflow_name = "in";

    while (current_workflow_name != "A" && current_workflow_name != "R") {
        auto it = workflows.find(current_workflow_name);
        // Assuming valid input guarantees workflow exists if not "A" or "R"
        const Workflow& current_workflow = it->second;

        std::string next_workflow;
        for (const auto& rule : current_workflow) {
            if (evaluate_rule(part, rule, next_workflow)) {
                current_workflow_name = next_workflow;
                break; // Rule applied, move to the next workflow
            }
        }
         // If no rule applied in a non-terminal workflow, something is wrong
         // but problem constraints likely prevent this.
    }

    return current_workflow_name == "A";
}

int main() {
    // Optimize C++ standard streams
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL); // Not strictly needed for file I/O but good practice

    std::ifstream infile("input.txt");
    if (!infile.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::unordered_map<std::string, Workflow> workflows;
    std::vector<Part> parts;
    std::string line;
    bool parsing_workflows = true;

    while (std::getline(infile, line)) {
        if (line.empty()) {
            parsing_workflows = false;
            continue;
        }

        if (parsing_workflows) {
            // Parse workflow: name{rule1,rule2,...}
            size_t brace_pos = line.find('{');
            std::string name = line.substr(0, brace_pos);
            std::string rules_str = line.substr(brace_pos + 1, line.length() - brace_pos - 2);

            Workflow workflow;
            std::stringstream ss_rules(rules_str);
            std::string rule_str;

            while (std::getline(ss_rules, rule_str, ',')) {
                Rule rule;
                size_t colon_pos = rule_str.find(':');
                if (colon_pos != std::string::npos) {
                    // Conditional rule: x<123:dest
                    rule.category_index = category_to_index(rule_str[0]);
                    rule.op = rule_str[1];
                    rule.value = std::stoi(rule_str.substr(2, colon_pos - 2));
                    rule.destination = rule_str.substr(colon_pos + 1);
                } else {
                    // Default rule: dest
                    rule.category_index = -1;
                    rule.op = 0; // Indicate no operation
                    rule.value = 0;
                    rule.destination = rule_str;
                }
                workflow.push_back(rule);
            }
            workflows[name] = workflow;
        } else {
            // Parse part: {x=1,m=2,a=3,s=4}
            Part part = {{0}}; // Initialize ratings to 0
            std::string ratings_str = line.substr(1, line.length() - 2);
            std::stringstream ss_ratings(ratings_str);
            std::string rating_str;

            while (std::getline(ss_ratings, rating_str, ',')) {
                 size_t eq_pos = rating_str.find('=');
                 char category = rating_str[0];
                 int value = std::stoi(rating_str.substr(eq_pos + 1));
                 part.ratings[category_to_index(category)] = value;
            }
            parts.push_back(part);
        }
    }

    long long total_sum_accepted_parts = 0;
    for (const auto& part : parts) {
        if (process_part(part, workflows)) {
            total_sum_accepted_parts += part.sum();
        }
    }

    std::cout << total_sum_accepted_parts << std::endl;

    return 0;
}
