
#include <string>
#include <vector>
#include <utility>
#include <set>
#include <map>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <regex>

struct Rule {
    std::string name;
    std::vector<std::pair<int, int>> ranges;

    bool isValid(int value) const {
        for (const auto& range : ranges) {
            if (value >= range.first && value <= range.second) {
                return true;
            }
        }
        return false;
    }
};

std::vector<int> parseTicket(const std::string& s) {
    std::vector<int> ticket;
    std::string current_num;
    for (char c : s) {
        if (c == ',') {
            ticket.push_back(std::stoi(current_num));
            current_num = "";
        } else {
            current_num += c;
        }
    }
    ticket.push_back(std::stoi(current_num));
    return ticket;
}

bool isValidTicket(const std::vector<int>& ticket, const std::vector<Rule>& rules) {
    for (int value : ticket) {
        bool value_is_valid_for_any_rule = false;
        for (const auto& rule : rules) {
            if (rule.isValid(value)) {
                value_is_valid_for_any_rule = true;
                break;
            }
        }
        if (!value_is_valid_for_any_rule) {
            return false;
        }
    }
    return true;
}

std::map<std::string, int> solveFieldPositions(const std::vector<Rule>& rules, const std::vector<std::vector<int>>& tickets) {
    std::map<std::string, std::set<int>> valid_positions;
    int num_fields = tickets[0].size();

    for (const auto& rule : rules) {
        for (int i = 0; i < num_fields; ++i) {
            valid_positions[rule.name].insert(i);
        }
    }

    for (const auto& ticket : tickets) {
        for (int idx = 0; idx < num_fields; ++idx) {
            int value = ticket[idx];
            for (const auto& rule : rules) {
                if (!rule.isValid(value)) {
                    valid_positions[rule.name].erase(idx);
                }
            }
        }
    }

    std::map<std::string, int> field_positions;
    while (field_positions.size() < rules.size()) {
        std::vector<std::string> resolved_this_iteration;
        for (const auto& pair : valid_positions) {
            if (pair.second.size() == 1) {
                resolved_this_iteration.push_back(pair.first);
            }
        }

        if (resolved_this_iteration.empty() && field_positions.size() < rules.size()) {
            break; // Should not happen in a valid puzzle input
        }

        for (const std::string& name : resolved_this_iteration) {
            int pos = *valid_positions[name].begin();
            field_positions[name] = pos;
            valid_positions.erase(name);

            for (auto& pair : valid_positions) {
                pair.second.erase(pos);
            }
        }
    }
    return field_positions;
}

long long calculateDepartureProduct(const std::vector<int>& ticket, const std::map<std::string, int>& field_positions) {
    long long product = 1;
    for (const auto& pair : field_positions) {
        if (pair.first.length() >= 9 && pair.first.compare(0, 9, "departure") == 0) {
            product *= ticket[pair.second];
        }
    }
    return product;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    std::vector<Rule> rules;
    std::vector<int> my_ticket;
    std::vector<std::vector<int>> nearby_tickets;

    int section = 0;
    std::string line;
    std::regex re_rule("^([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$");

    while (std::getline(file, line)) {
        if (line.empty()) {
            section++;
            continue;
        }

        if (section == 0) {
            std::smatch match;
            if (std::regex_match(line, match, re_rule)) {
                rules.push_back({match[1].str(), {{std::stoi(match[2]), std::stoi(match[3])}, {std::stoi(match[4]), std::stoi(match[5])}}});
            }
        } else if (section == 1) {
            if (line != "your ticket:") {
                my_ticket = parseTicket(line);
            }
        } else if (section == 2) {
            if (line != "nearby tickets:") {
                std::vector<int> ticket = parseTicket(line);
                if (isValidTicket(ticket, rules)) {
                    nearby_tickets.push_back(ticket);
                }
            }
        }
    }

    std::map<std::string, int> field_positions = solveFieldPositions(rules, nearby_tickets);
    long long departure_product = calculateDepartureProduct(my_ticket, field_positions);

    std::cout << departure_product << std::endl;

    return 0;
}
