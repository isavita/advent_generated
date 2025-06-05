
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <queue>
#include <tuple>
#include <limits>
#include <fstream>
#include <algorithm>

// Constants
const int GEOLOGIC_Y_MULTIPLIER = 16807;
const int GEOLOGIC_X_MULTIPLIER = 48271;
const int CAVE_MODULO = 20183;

// Region Types
const int TYPE_ROCKY = 0;
const int TYPE_WET = 1;
const int TYPE_NARROW = 2;

// Tool Types (bitmasks)
const int TOOL_NONE = 1;  // 001
const int TOOL_TORCH = 2; // 010
const int TOOL_GEAR = 4;  // 100

// Forward declaration for allowed function
int allowed(int region_type);

class Map {
public:
    int depth;
    int targetX;
    int targetY;

    std::vector<std::vector<int>> geologic_indices_cache;
    std::vector<std::vector<int>> erosion_levels_cache;

    int max_map_x_coord;
    int max_map_y_coord;

    Map(const std::string& input_str) {
        std::stringstream ss(input_str);
        std::string line;

        std::getline(ss, line);
        depth = std::stoi(line.substr(line.find(": ") + 2));

        std::getline(ss, line);
        size_t comma_pos = line.find(",");
        targetX = std::stoi(line.substr(line.find(": ") + 2, comma_pos - (line.find(": ") + 2)));
        targetY = std::stoi(line.substr(comma_pos + 1));

        // Initialize caches based on the heuristic of 8x target coords + a buffer
        max_map_x_coord = targetX * 8 + 10;
        max_map_y_coord = targetY * 8 + 10;
        
        // Ensure minimum size for initial coords (0,0) and target
        max_map_x_coord = std::max(max_map_x_coord, targetX + 1);
        max_map_y_coord = std::max(max_map_y_coord, targetY + 1);

        geologic_indices_cache.assign(max_map_y_coord, std::vector<int>(max_map_x_coord, -1));
        erosion_levels_cache.assign(max_map_y_coord, std::vector<int>(max_map_x_coord, -1));
    }

    int geologic_index(int x, int y) {
        if (x < 0 || y < 0 || x >= max_map_x_coord || y >= max_map_y_coord) {
            return -1; // Should not happen with proper bounds
        }

        if (geologic_indices_cache[y][x] != -1) {
            return geologic_indices_cache[y][x];
        }

        long long index;
        if (x == 0 && y == 0) {
            index = 0;
        } else if (x == targetX && y == targetY) {
            index = 0;
        } else if (y == 0) {
            index = (long long)x * GEOLOGIC_Y_MULTIPLIER;
        } else if (x == 0) {
            index = (long long)y * GEOLOGIC_X_MULTIPLIER;
        } else {
            index = (long long)erosion_level(x - 1, y) * erosion_level(x, y - 1);
        }
        geologic_indices_cache[y][x] = static_cast<int>(index);
        return geologic_indices_cache[y][x];
    }

    int erosion_level(int x, int y) {
        if (x < 0 || y < 0 || x >= max_map_x_coord || y >= max_map_y_coord) {
            return -1; // Should not happen
        }

        if (erosion_levels_cache[y][x] != -1) {
            return erosion_levels_cache[y][x];
        }

        long long level = (long long)geologic_index(x, y) + depth;
        erosion_levels_cache[y][x] = static_cast<int>(level % CAVE_MODULO);
        return erosion_levels_cache[y][x];
    }

    int get_type(int x, int y) {
        return erosion_level(x, y) % 3;
    }

    struct Neighbor {
        int x, y, equip, cost;
    };

    std::vector<Neighbor> get_neighbors(int x, int y, int equip) {
        std::vector<Neighbor> neighbors_list;
        int dx[] = {1, 0, -1, 0};
        int dy[] = {0, 1, 0, -1};

        for (int i = 0; i < 4; ++i) {
            int nx = x + dx[i];
            int ny = y + dy[i];

            if (nx >= 0 && ny >= 0) {
                // If the neighbor is outside the pre-allocated cache bounds, skip.
                // This means the search went unexpectedly far.
                if (nx >= max_map_x_coord || ny >= max_map_y_coord) {
                    continue; 
                }

                int region_type = get_type(nx, ny);
                
                if (equip & allowed(region_type)) {
                    neighbors_list.push_back({nx, ny, equip, 1}); // Move cost 1

                    // Change equipment: current equipment XOR allowed tools for region_type
                    neighbors_list.push_back({nx, ny, equip ^ allowed(region_type), 8}); // Change tool cost 7 + move cost 1 = 8
                }
            }
        }
        return neighbors_list;
    }
};

int allowed(int region_type) {
    if (region_type == TYPE_ROCKY) {
        return TOOL_GEAR | TOOL_TORCH;
    } else if (region_type == TYPE_WET) {
        return TOOL_GEAR | TOOL_NONE;
    } else if (region_type == TYPE_NARROW) {
        return TOOL_TORCH | TOOL_NONE;
    }
    return 0; 
}

int rescue(const std::string& input_str) {
    Map m(input_str);

    using State = std::tuple<int, int, int, int>; // (time, x, y, equip)
    std::priority_queue<State, std::vector<State>, std::greater<State>> pq;

    using DistKey = std::tuple<int, int, int>; // (x, y, equip)
    std::map<DistKey, int> distances;

    pq.push({0, 0, 0, TOOL_TORCH});
    distances[{0, 0, TOOL_TORCH}] = 0;

    int max_target_x_heuristic = m.targetX * 8;
    int max_target_y_heuristic = m.targetY * 8;

    while (!pq.empty()) {
        auto [time, x, y, equip] = pq.top();
        pq.pop();

        if (x == m.targetX && y == m.targetY && equip == TOOL_TORCH) {
            return time;
        }

        // Pruning heuristic from the Python solution
        if (x > max_target_x_heuristic || y > max_target_y_heuristic) {
             continue;
        }
        
        if (time > distances[{x, y, equip}]) {
            continue;
        }

        for (const auto& neighbor : m.get_neighbors(x, y, equip)) {
            int new_time = time + neighbor.cost;
            DistKey new_key = {neighbor.x, neighbor.y, neighbor.equip};

            if (distances.find(new_key) == distances.end() || new_time < distances[new_key]) {
                distances[new_key] = new_time;
                pq.push({new_time, neighbor.x, neighbor.y, neighbor.equip});
            }
        }
    }

    return 0;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string input_data = buffer.str();
    file.close();

    std::cout << rescue(input_data) << std::endl;

    return 0;
}
