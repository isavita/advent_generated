
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <map>
#include <array>
#include <numeric>
#include <algorithm>

using Grid = std::array<bool, 25>;
using Space = std::map<int, Grid>;

Grid parse() {
    Grid res{};
    std::ifstream file("input.txt");
    std::string line;
    int row = 0;
    while (std::getline(file, line) && row < 5) {
        for (int col = 0; col < 5; ++col) {
            if (col < line.length() && line[col] == '#') {
                res[row * 5 + col] = true;
            }
        }
        row++;
    }
    return res;
}

bool is_infested(const Space& space, int level, int cell) {
    auto it = space.find(level);
    if (it == space.end()) {
        return false;
    }
    return it->second[cell];
}

void clean_space(Space& space) {
    if (space.empty()) return;

    int min_level = space.begin()->first;
    bool min_level_empty = true;
    for (bool val : space.at(min_level)) {
        if (val) {
            min_level_empty = false;
            break;
        }
    }
    if (min_level_empty) {
        space.erase(min_level);
    }

    if (space.empty()) return;

    int max_level = space.rbegin()->first;
    bool max_level_empty = true;
    for (bool val : space.at(max_level)) {
        if (val) {
            max_level_empty = false;
            break;
        }
    }
    if (max_level_empty) {
        space.erase(max_level);
    }
}

Space next_state(const Space& current_space) {
    Space new_space;

    int min_l = current_space.begin()->first;
    int max_l = current_space.rbegin()->first;

    for (int level = min_l - 1; level <= max_l + 1; ++level) {
        Grid current_grid = current_space.count(level) ? current_space.at(level) : Grid{};
        Grid next_grid = current_grid;

        for (int cell = 0; cell < 25; ++cell) {
            if (cell == 12) {
                continue;
            }

            int row = cell / 5;
            int col = cell % 5;
            int neighbours = 0;

            if (row == 0) {
                if (is_infested(current_space, level - 1, 7)) neighbours++;
            }
            if (col == 0) {
                if (is_infested(current_space, level - 1, 11)) neighbours++;
            }
            if (col == 4) {
                if (is_infested(current_space, level - 1, 13)) neighbours++;
            }
            if (row == 4) {
                if (is_infested(current_space, level - 1, 17)) neighbours++;
            }

            if (cell == 7) {
                for (int i = 0; i < 5; ++i) {
                    if (is_infested(current_space, level + 1, i)) neighbours++;
                }
            }
            if (cell == 11) {
                for (int i = 0; i < 5; ++i) {
                    if (is_infested(current_space, level + 1, 5 * i)) neighbours++;
                }
            }
            if (cell == 13) {
                for (int i = 0; i < 5; ++i) {
                    if (is_infested(current_space, level + 1, 5 * i + 4)) neighbours++;
                }
            }
            if (cell == 17) {
                for (int i = 0; i < 5; ++i) {
                    if (is_infested(current_space, level + 1, 20 + i)) neighbours++;
                }
            }

            if (row > 0 && cell - 5 != 12) {
                if (is_infested(current_space, level, cell - 5)) neighbours++;
            }
            if (col > 0 && cell - 1 != 12) {
                if (is_infested(current_space, level, cell - 1)) neighbours++;
            }
            if (col < 4 && cell + 1 != 12) {
                if (is_infested(current_space, level, cell + 1)) neighbours++;
            }
            if (row < 4 && cell + 5 != 12) {
                if (is_infested(current_space, level, cell + 5)) neighbours++;
            }
            
            bool current_state = is_infested(current_space, level, cell);

            if (current_state && neighbours != 1) {
                next_grid[cell] = false;
            } else if (!current_state && (neighbours == 1 || neighbours == 2)) {
                next_grid[cell] = true;
            } else {
                next_grid[cell] = current_state;
            }
        }
        new_space[level] = next_grid;
    }

    clean_space(new_space);

    return new_space;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    Grid initial_data = parse();

    Space space;
    space[0] = initial_data;

    for (int i = 0; i < 200; ++i) {
        space = next_state(space);
    }

    long long total_infested_count = 0;
    for (const auto& pair : space) {
        total_infested_count += std::accumulate(pair.second.begin(), pair.second.end(), 0LL);
    }

    std::cout << total_infested_count << std::endl;

    return 0;
}
