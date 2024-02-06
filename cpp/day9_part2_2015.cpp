
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>

std::unordered_map<std::string, std::unordered_map<std::string, int> > readAndParseInput(std::string filename);
std::vector<std::string> getUniqueLocations(std::unordered_map<std::string, std::unordered_map<std::string, int> > distances);
int findLongestRoute(std::vector<std::string> locations, std::unordered_map<std::string, std::unordered_map<std::string, int> > distances);
void permute(std::vector<std::string> arr, int i, int* bestDistance, std::unordered_map<std::string, std::unordered_map<std::string, int> > distances, bool findShortest);
int calculateRouteDistance(std::vector<std::string> route, std::unordered_map<std::string, std::unordered_map<std::string, int> > distances);

int main() {
    std::unordered_map<std::string, std::unordered_map<std::string, int> > distances = readAndParseInput("input.txt");
    std::vector<std::string> locations = getUniqueLocations(distances);
    int maxDistance = findLongestRoute(locations, distances);
    std::cout << maxDistance << std::endl;
    return 0;
}

std::unordered_map<std::string, std::unordered_map<std::string, int> > readAndParseInput(std::string filename) {
    std::unordered_map<std::string, std::unordered_map<std::string, int> > distances;
    std::ifstream file(filename);
    if (!file.is_open()) {
        return distances;
    }

    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::vector<std::string> parts(std::istream_iterator<std::string>{iss}, std::istream_iterator<std::string>());

        if (parts.size() != 5) {
            continue; // Invalid line format
        }

        std::string from = parts[0];
        std::string to = parts[2];
        int distance = std::stoi(parts[4]);

        distances[from][to] = distance;

        distances[to][from] = distance; // Assuming distance is symmetric
    }

    file.close();
    return distances;
}

std::vector<std::string> getUniqueLocations(std::unordered_map<std::string, std::unordered_map<std::string, int> > distances) {
    std::unordered_map<std::string, int> locationSet;
    for (const auto& pair : distances) {
        locationSet[pair.first] = 1;
        for (const auto& innerPair : pair.second) {
            locationSet[innerPair.first] = 1;
        }
    }

    std::vector<std::string> locations;
    for (const auto& pair : locationSet) {
        locations.push_back(pair.first);
    }

    return locations;
}

int findLongestRoute(std::vector<std::string> locations, std::unordered_map<std::string, std::unordered_map<std::string, int> > distances) {
    int maxDistance = 0;
    permute(locations, 0, &maxDistance, distances, false);
    return maxDistance;
}

void permute(std::vector<std::string> arr, int i, int* bestDistance, std::unordered_map<std::string, std::unordered_map<std::string, int> > distances, bool findShortest) {
    if (i > arr.size()) {
        return;
    }
    if (i == arr.size()) {
        int dist = calculateRouteDistance(arr, distances);
        if (findShortest) {
            if (*bestDistance == 0 || dist < *bestDistance) {
                *bestDistance = dist;
            }
        } else {
            if (dist > *bestDistance) {
                *bestDistance = dist;
            }
        }
        return;
    }
    for (int j = i; j < arr.size(); j++) {
        std::swap(arr[i], arr[j]);
        permute(arr, i+1, bestDistance, distances, findShortest);
        std::swap(arr[i], arr[j]);
    }
}

int calculateRouteDistance(std::vector<std::string> route, std::unordered_map<std::string, std::unordered_map<std::string, int> > distances) {
    int sum = 0;
    for (size_t i = 0; i < route.size()-1; i++) {
        sum += distances[route[i]][route[i+1]];
    }
    return sum;
}
