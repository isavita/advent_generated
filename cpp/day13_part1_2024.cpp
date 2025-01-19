
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <algorithm>

struct Machine {
    int ax, ay, bx, by, px, py;
};

std::vector<Machine> readInput(const std::string& filename) {
    std::ifstream f(filename);
    std::vector<Machine> machines;
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(f, line)) {
        line.erase(0, line.find_first_not_of(" \t\n\r\f\v"));
        line.erase(line.find_last_not_of(" \t\n\r\f\v") + 1);
        if (line.empty()) {
            if (!lines.empty()) {
                Machine m;
                for (const std::string& l : lines) {
                    std::string temp = l;
                    if (temp.find("Button A:") != std::string::npos) temp.replace(temp.find("Button A:"), 9, "A:");
                    if (temp.find("Button B:") != std::string::npos) temp.replace(temp.find("Button B:"), 9, "B:");
                    if (temp.find("Prize:") != std::string::npos) temp.replace(temp.find("Prize:"), 6, "P:");

                    if (temp.rfind("A:", 0) == 0) {
                        std::stringstream ss(temp.substr(2));
                        std::string part;
                        std::getline(ss, part, ',');
                        part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                        part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                        if (part.rfind("X+", 0) == 0) part = part.substr(2);
                        if (part.rfind("X=", 0) == 0) part = part.substr(2);
                        m.ax = std::stoi(part);
                        std::getline(ss, part, ',');
                        part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                        part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                        if (part.rfind("Y+", 0) == 0) part = part.substr(2);
                        if (part.rfind("Y=", 0) == 0) part = part.substr(2);
                        m.ay = std::stoi(part);
                    } else if (temp.rfind("B:", 0) == 0) {
                        std::stringstream ss(temp.substr(2));
                        std::string part;
                        std::getline(ss, part, ',');
                        part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                        part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                        if (part.rfind("X+", 0) == 0) part = part.substr(2);
                        if (part.rfind("X=", 0) == 0) part = part.substr(2);
                        m.bx = std::stoi(part);
                        std::getline(ss, part, ',');
                        part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                        part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                        if (part.rfind("Y+", 0) == 0) part = part.substr(2);
                        if (part.rfind("Y=", 0) == 0) part = part.substr(2);
                        m.by = std::stoi(part);
                    } else if (temp.rfind("P:", 0) == 0) {
                        std::stringstream ss(temp.substr(2));
                        std::string part;
                        std::getline(ss, part, ',');
                        part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                        part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                        if (part.rfind("X=", 0) == 0) part = part.substr(2);
                        m.px = std::stoi(part);
                        std::getline(ss, part, ',');
                        part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                        part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                        if (part.rfind("Y=", 0) == 0) part = part.substr(2);
                        m.py = std::stoi(part);
                    }
                }
                machines.push_back(m);
                lines.clear();
            }
        } else {
            lines.push_back(line);
        }
    }
    if (!lines.empty()) {
        Machine m;
        for (const std::string& l : lines) {
            std::string temp = l;
            if (temp.find("Button A:") != std::string::npos) temp.replace(temp.find("Button A:"), 9, "A:");
            if (temp.find("Button B:") != std::string::npos) temp.replace(temp.find("Button B:"), 9, "B:");
            if (temp.find("Prize:") != std::string::npos) temp.replace(temp.find("Prize:"), 6, "P:");

            if (temp.rfind("A:", 0) == 0) {
                std::stringstream ss(temp.substr(2));
                std::string part;
                std::getline(ss, part, ',');
                part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                if (part.rfind("X+", 0) == 0) part = part.substr(2);
                if (part.rfind("X=", 0) == 0) part = part.substr(2);
                m.ax = std::stoi(part);
                std::getline(ss, part, ',');
                part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                if (part.rfind("Y+", 0) == 0) part = part.substr(2);
                if (part.rfind("Y=", 0) == 0) part = part.substr(2);
                m.ay = std::stoi(part);
            } else if (temp.rfind("B:", 0) == 0) {
                std::stringstream ss(temp.substr(2));
                std::string part;
                std::getline(ss, part, ',');
                part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                if (part.rfind("X+", 0) == 0) part = part.substr(2);
                if (part.rfind("X=", 0) == 0) part = part.substr(2);
                m.bx = std::stoi(part);
                std::getline(ss, part, ',');
                part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                if (part.rfind("Y+", 0) == 0) part = part.substr(2);
                if (part.rfind("Y=", 0) == 0) part = part.substr(2);
                m.by = std::stoi(part);
            } else if (temp.rfind("P:", 0) == 0) {
                std::stringstream ss(temp.substr(2));
                std::string part;
                std::getline(ss, part, ',');
                part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                if (part.rfind("X=", 0) == 0) part = part.substr(2);
                m.px = std::stoi(part);
                std::getline(ss, part, ',');
                part.erase(0, part.find_first_not_of(" \t\n\r\f\v"));
                part.erase(part.find_last_not_of(" \t\n\r\f\v") + 1);
                if (part.rfind("Y=", 0) == 0) part = part.substr(2);
                m.py = std::stoi(part);
            }
        }
        machines.push_back(m);
    }
    return machines;
}

int solveMachine(const Machine& m) {
    int minCost = -1;
    for (int aCount = 0; aCount <= 100; ++aCount) {
        for (int bCount = 0; bCount <= 100; ++bCount) {
            int x = m.ax * aCount + m.bx * bCount;
            int y = m.ay * aCount + m.by * bCount;
            if (x == m.px && y == m.py) {
                int cost = aCount * 3 + bCount;
                if (minCost == -1 || cost < minCost) {
                    minCost = cost;
                }
            }
        }
    }
    return minCost;
}

int main() {
    std::vector<Machine> machines = readInput("input.txt");
    std::vector<int> results;
    for (const auto& m : machines) {
        int cost = solveMachine(m);
        if (cost >= 0) {
            results.push_back(cost);
        }
    }
    if (results.empty()) {
        std::cout << "0 0" << std::endl;
        return 0;
    }
    int count = results.size();
    int sum = 0;
    for (int c : results) {
        sum += c;
    }
    std::cout << count << " " << sum << std::endl;
    return 0;
}
