
#include <iostream>
#include <fstream>
#include <vector>

struct Component {
    int a, b;
};

int maxStrength = 0;
int maxLength = 0;

void findStrongestLongestBridge(std::vector<Component>& components, std::vector<bool>& used, int port, int strength, int length);

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input file" << std::endl;
        return 1;
    }

    std::vector<Component> components;
    std::string line;
    while (std::getline(file, line)) {
        std::string port1, port2;
        std::size_t pos = line.find("/");
        port1 = line.substr(0, pos);
        port2 = line.substr(pos + 1);
        components.push_back({std::stoi(port1), std::stoi(port2)});
    }

    std::vector<bool> used(components.size(), false);
    findStrongestLongestBridge(components, used, 0, 0, 0);

    std::cout << maxStrength << std::endl;

    return 0;
}

void findStrongestLongestBridge(std::vector<Component>& components, std::vector<bool>& used, int port, int strength, int length) {
    if (length > maxLength || (length == maxLength && strength > maxStrength)) {
        maxStrength = strength;
        maxLength = length;
    }

    for (size_t i = 0; i < components.size(); ++i) {
        if (used[i]) {
            continue;
        }

        Component c = components[i];
        if (c.a == port || c.b == port) {
            used[i] = true;
            int nextPort = c.a;
            if (c.a == port) {
                nextPort = c.b;
            }
            findStrongestLongestBridge(components, used, nextPort, strength + c.a + c.b, length + 1);
            used[i] = false;
        }
    }
}
