
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm> // For std::reverse
#include <numeric>   // For std::accumulate (though manual multiplication is simple)

std::vector<std::string> get_borders(const std::vector<std::string>& tile) {
    std::vector<std::string> borders;
    if (tile.empty()) {
        return borders;
    }

    borders.push_back(tile[0]);
    borders.push_back(tile[tile.size() - 1]);

    std::string left_border;
    std::string right_border;
    for (const auto& row : tile) {
        if (!row.empty()) {
            left_border += row[0];
            right_border += row[row.size() - 1];
        }
    }
    borders.push_back(left_border);
    borders.push_back(right_border);

    return borders;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt\n";
        return 1;
    }

    std::map<int, std::vector<std::string>> tiles;
    int current_tile_id = -1;
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) {
            continue;
        }

        if (line.rfind("Tile ", 0) == 0) {
            size_t start = line.find(" ") + 1;
            size_t end = line.find(":");
            current_tile_id = std::stoi(line.substr(start, end - start));
            tiles[current_tile_id] = std::vector<std::string>();
        } else {
            if (current_tile_id != -1) {
                tiles[current_tile_id].push_back(line);
            }
        }
    }
    file.close();

    std::map<std::string, std::vector<int>> borders_map;
    for (const auto& pair : tiles) {
        int tile_id = pair.first;
        const std::vector<std::string>& tile_data = pair.second;

        std::vector<std::string> current_borders = get_borders(tile_data);
        for (const std::string& border : current_borders) {
            borders_map[border].push_back(tile_id);
            
            std::string reversed_border = border;
            std::reverse(reversed_border.begin(), reversed_border.end());
            borders_map[reversed_border].push_back(tile_id);
        }
    }

    std::vector<long long> corner_tile_ids;
    for (const auto& pair : tiles) {
        int tile_id = pair.first;
        const std::vector<std::string>& tile_data = pair.second;

        int unique_border_count = 0;
        std::vector<std::string> current_borders = get_borders(tile_data);
        
        for (const std::string& border : current_borders) {
            if (borders_map[border].size() == 1) {
                unique_border_count++;
            }
        }
        
        if (unique_border_count == 2) {
            corner_tile_ids.push_back(tile_id);
        }
    }

    long long product = 1;
    for (long long id : corner_tile_ids) {
        product *= id;
    }

    std::cout << product << std::endl;

    return 0;
}

