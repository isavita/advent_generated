
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <limits>
#include <sstream>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    std::map<std::pair<std::string, std::string>, int> distances;
    std::set<std::string> locations_set;
    
    std::string line;
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string loc1, to, loc2, eq;
        int dist;
        ss >> loc1 >> to >> loc2 >> eq >> dist;
        
        distances[{loc1, loc2}] = dist;
        distances[{loc2, loc1}] = dist;
        locations_set.insert(loc1);
        locations_set.insert(loc2);
    }
    file.close();

    std::vector<std::string> locations(locations_set.begin(), locations_set.end());
    
    int min_total_distance = std::numeric_limits<int>::max();

    std::sort(locations.begin(), locations.end());

    do {
        int current_total_distance = 0;
        for (size_t i = 0; i < locations.size() - 1; ++i) {
            current_total_distance += distances.at({locations[i], locations[i+1]});
        }
        min_total_distance = std::min(min_total_distance, current_total_distance);
    } while (std::next_permutation(locations.begin(), locations.end()));

    std::cout << min_total_distance << std::endl;

    return 0;
}

