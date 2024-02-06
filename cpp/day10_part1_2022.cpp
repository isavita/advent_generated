
#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

std::string readAll(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int main() {
    std::vector<int> x = {1};
    std::string input = readAll("input.txt");
    std::stringstream ss(input);
    std::string line;
    
    while (std::getline(ss, line)) {
        if (line == "noop") {
            x.push_back(x.back());
        } else {
            int n;
            std::sscanf(line.c_str(), "addx %d", &n);
            x.push_back(x.back());
            x.push_back(x.back() + n);
        }
    }

    int sum = 0;
    for (int i = 0; i < x.size(); ++i) {
        if ((i - 19) % 40 == 0) {
            sum += (i + 1) * x[i];
        }
    }
    std::cout << sum << std::endl;
    
    return 0;
}
