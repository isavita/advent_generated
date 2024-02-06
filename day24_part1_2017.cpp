
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

struct Component {
    int a, b;
};

int maxStrength = 0;

void findStrongestBridge(std::vector<Component>& components, std::vector<bool>& used, int port, int strength);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }

    std::vector<Component> components;
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string portA, portB;
        std::getline(iss, portA, '/');
        std::getline(iss, portB, '/');
        int a = std::stoi(portA);
        int b = std::stoi(portB);
        components.push_back({a, b});
    }

    std::vector<bool> used(components.size(), false);
    findStrongestBridge(components, used, 0, 0);

    std::cout << maxStrength << std::endl;
    return 0;
}

void findStrongestBridge(std::vector<Component>& components, std::vector<bool>& used, int port, int strength) {
    if (strength > maxStrength) {
        maxStrength = strength;
    }

    for (int i = 0; i < components.size(); ++i) {
        if (used[i]) {
            continue;
        }

        if (components[i].a == port || components[i].b == port) {
            used[i] = true;
            int nextPort = components[i].a;
            if (components[i].a == port) {
                nextPort = components[i].b;
            }
            findStrongestBridge(components, used, nextPort, strength + components[i].a + components[i].b);
            used[i] = false;
        }
    }
}
