
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <queue>
#include <limits> // For numeric_limits
#include <cstdio> // For sscanf

struct Coord {
    int x, y, z;
};

const Coord neighbors[] = {
    {-1, 0, 0}, {1, 0, 0},
    {0, -1, 0}, {0, 1, 0},
    {0, 0, -1}, {0, 0, 1}
};

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<Coord> lava_cubes_list;
    int min_x = std::numeric_limits<int>::max();
    int min_y = std::numeric_limits<int>::max();
    int min_z = std::numeric_limits<int>::max();
    int max_x = std::numeric_limits<int>::min();
    int max_y = std::numeric_limits<int>::min();
    int max_z = std::numeric_limits<int>::min();

    std::ifstream file("input.txt");
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) continue;
        
        Coord cube;
        if (sscanf(line.c_str(), "%d,%d,%d", &cube.x, &cube.y, &cube.z) == 3) {
            lava_cubes_list.push_back(cube);
            min_x = std::min(min_x, cube.x);
            min_y = std::min(min_y, cube.y);
            min_z = std::min(min_z, cube.z);
            max_x = std::max(max_x, cube.x);
            max_y = std::max(max_y, cube.y);
            max_z = std::max(max_z, cube.z);
        }
    }
    file.close();

    min_x--; min_y--; min_z--;
    max_x++; max_y++; max_z++;

    int dim_x = max_x - min_x + 1;
    int dim_y = max_y - min_y + 1;
    int dim_z = max_z - min_z + 1;

    std::vector<std::vector<std::vector<char>>> grid(dim_x, std::vector<std::vector<char>>(dim_y, std::vector<char>(dim_z, 0)));

    for (const auto& cube : lava_cubes_list) {
        grid[cube.x - min_x][cube.y - min_y][cube.z - min_z] = 1;
    }

    int faces = 0;
    std::queue<Coord> q;

    Coord start_node = {0, 0, 0};
    q.push(start_node);
    grid[start_node.x][start_node.y][start_node.z] = 2;

    while (!q.empty()) {
        Coord curr = q.front();
        q.pop();

        for (const auto& delta : neighbors) {
            Coord next_pos_shifted = {curr.x + delta.x, curr.y + delta.y, curr.z + delta.z};

            if (next_pos_shifted.x < 0 || next_pos_shifted.x >= dim_x ||
                next_pos_shifted.y < 0 || next_pos_shifted.y >= dim_y ||
                next_pos_shifted.z < 0 || next_pos_shifted.z >= dim_z) {
                continue;
            }

            char cell_type = grid[next_pos_shifted.x][next_pos_shifted.y][next_pos_shifted.z];

            if (cell_type == 1) {
                faces++;
            } else if (cell_type == 0) {
                grid[next_pos_shifted.x][next_pos_shifted.y][next_pos_shifted.z] = 2;
                q.push(next_pos_shifted);
            }
        }
    }

    std::cout << faces << std::endl;

    return 0;
}
