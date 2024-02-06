
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <algorithm>

int calculateHappiness(std::vector<std::string> arrangement, std::unordered_map<std::string, std::unordered_map<std::string, int> > happinessMap);
void permute(std::vector<std::string>& arr, int i, int& maxHappiness, std::unordered_map<std::string, std::unordered_map<std::string, int> > happinessMap);

int main() {
    std::ifstream file("input.txt");
    std::unordered_map<std::string, std::unordered_map<std::string, int> > happinessMap;

    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::vector<std::string> parts(std::istream_iterator<std::string>{iss}, std::istream_iterator<std::string>());

        if (parts.size() < 11) {
            continue;
        }

        std::string from = parts[0];
        std::string to = parts[10].substr(0, parts[10].size() - 1);
        int change = std::stoi(parts[3]);
        if (parts[2] == "lose") {
            change = -change;
        }

        if (happinessMap[from].empty()) {
            happinessMap[from] = std::unordered_map<std::string, int>();
        }
        happinessMap[from][to] = change;
    }

    std::vector<std::string> guests;
    for (auto const& entry : happinessMap) {
        guests.push_back(entry.first);
    }

    int maxHappiness = 0;
    permute(guests, 0, maxHappiness, happinessMap);

    std::cout << maxHappiness << std::endl;

    return 0;
}

int calculateHappiness(std::vector<std::string> arrangement, std::unordered_map<std::string, std::unordered_map<std::string, int> > happinessMap) {
    int happiness = 0;
    int n = arrangement.size();
    for (int i = 0; i < n; i++) {
        int left = (i + n - 1) % n;
        int right = (i + 1) % n;
        happiness += happinessMap[arrangement[i]][arrangement[left]];
        happiness += happinessMap[arrangement[i]][arrangement[right]];
    }
    return happiness;
}

void permute(std::vector<std::string>& arr, int i, int& maxHappiness, std::unordered_map<std::string, std::unordered_map<std::string, int> > happinessMap) {
    if (i > arr.size()) {
        return;
    }
    if (i == arr.size()) {
        int happiness = calculateHappiness(arr, happinessMap);
        if (happiness > maxHappiness) {
            maxHappiness = happiness;
        }
        return;
    }
    for (int j = i; j < arr.size(); j++) {
        std::swap(arr[i], arr[j]);
        permute(arr, i+1, maxHappiness, happinessMap);
        std::swap(arr[i], arr[j]);
    }
}
