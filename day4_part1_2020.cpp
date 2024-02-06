#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

bool isValid(std::string passport, std::vector<std::string> requiredFields);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> passports;
    std::string passport = "";
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) {
            passports.push_back(passport);
            passport = "";
        } else {
            passport += " " + line;
        }
    }
    if (!passport.empty()) {
        passports.push_back(passport);
    }

    int validPassports = 0;
    std::vector<std::string> requiredFields = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"};

    for (const auto& p : passports) {
        if (isValid(p, requiredFields)) {
            validPassports++;
        }
    }

    std::cout << validPassports << std::endl;

    return 0;
}

bool isValid(std::string passport, std::vector<std::string> requiredFields) {
    for (const auto& field : requiredFields) {
        if (passport.find(field + ":") == std::string::npos) {
            return false;
        }
    }
    return true;
}