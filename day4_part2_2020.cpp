
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <regex>

bool validateYear(std::string value, int min, int max) {
    try {
        int year = std::stoi(value);
        return year >= min && year <= max;
    } catch (...) {
        return false;
    }
}

bool validateByr(std::string value) {
    return validateYear(value, 1920, 2002);
}

bool validateIyr(std::string value) {
    return validateYear(value, 2010, 2020);
}

bool validateEyr(std::string value) {
    return validateYear(value, 2020, 2030);
}

bool validateHgt(std::string value) {
    if (value.find("cm") != std::string::npos) {
        int hgt = std::stoi(value.substr(0, value.size() - 2));
        return hgt >= 150 && hgt <= 193;
    } else if (value.find("in") != std::string::npos) {
        int hgt = std::stoi(value.substr(0, value.size() - 2));
        return hgt >= 59 && hgt <= 76;
    }
    return false;
}

bool validateHcl(std::string value) {
    std::regex pattern("^#[0-9a-f]{6}$");
    return std::regex_match(value, pattern);
}

bool validateEcl(std::string value) {
    std::unordered_map<std::string, bool> validEcl = {{"amb", true}, {"blu", true}, {"brn", true}, {"gry", true}, {"grn", true}, {"hzl", true}, {"oth", true}};
    return validEcl.find(value) != validEcl.end();
}

bool validatePid(std::string value) {
    std::regex pattern("^[0-9]{9}$");
    return std::regex_match(value, pattern);
}

bool isValidPassport(std::string passport) {
    std::istringstream iss(passport);
    std::string field;
    std::unordered_map<std::string, std::string> fieldMap;
    while (iss >> field) {
        size_t pos = field.find(":");
        if (pos != std::string::npos) {
            std::string key = field.substr(0, pos);
            std::string val = field.substr(pos + 1);
            fieldMap[key] = val;
        }
    }

    return validateByr(fieldMap["byr"]) &&
           validateIyr(fieldMap["iyr"]) &&
           validateEyr(fieldMap["eyr"]) &&
           validateHgt(fieldMap["hgt"]) &&
           validateHcl(fieldMap["hcl"]) &&
           validateEcl(fieldMap["ecl"]) &&
           validatePid(fieldMap["pid"]);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<std::string> passports;
    std::string line, passport;
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
    for (const auto& p : passports) {
        if (isValidPassport(p)) {
            validPassports++;
        }
    }

    std::cout << validPassports << std::endl;

    return 0;
}
