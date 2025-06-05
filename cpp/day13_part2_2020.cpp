
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <utility>

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1;
    }

    std::string line;
    std::getline(inputFile, line); // Discard first line
    std::getline(inputFile, line); // Read bus IDs line

    std::vector<std::pair<long long, long long>> buses;
    std::stringstream ss(line);
    std::string segment;
    long long offset = 0;

    while (std::getline(ss, segment, ',')) {
        if (segment != "x") {
            long long bus_id = std::stoll(segment);
            buses.push_back({bus_id, offset});
        }
        offset++;
    }

    inputFile.close();

    long long current_t = 1;
    long long current_step = 1;

    for (const auto& bus_info : buses) {
        long long bus = bus_info.first;
        long long offset_val = bus_info.second;

        while ((current_t + offset_val) % bus != 0) {
            current_t += current_step;
        }
        current_step *= bus;
    }

    std::cout << current_t << std::endl;

    return 0;
}

