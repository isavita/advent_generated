
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <deque>
#include <algorithm>

enum DirectionEnum { RIGHT = 0, LEFT = 1, DOWN = 2, UP = 3, NUM_DIRECTIONS = 4 };

const int DX[] = {1, -1, 0, 0};
const int DY[] = {0, 0, 1, -1};

struct Point {
    int x, y;
};

struct BeamState {
    Point pos;
    DirectionEnum dir;
};

std::vector<std::string> read_input(const std::string& file_path) {
    std::vector<std::string> grid;
    std::ifstream file(file_path);
    std::string line;
    while (std::getline(file, line)) {
        grid.push_back(line);
    }
    return grid;
}

DirectionEnum reflect(DirectionEnum dir, char mirror) {
    if (mirror == '/') {
        if (dir == RIGHT) return UP;
        if (dir == LEFT) return DOWN;
        if (dir == DOWN) return LEFT;
        if (dir == UP) return RIGHT;
    } else if (mirror == '\\') {
        if (dir == RIGHT) return DOWN;
        if (dir == LEFT) return UP;
        if (dir == DOWN) return RIGHT;
        if (dir == UP) return LEFT;
    }
    return dir;
}

std::vector<DirectionEnum> split_beam(DirectionEnum dir, char splitter) {
    if (splitter == '|') {
        if (dir == RIGHT || dir == LEFT) {
            return {UP, DOWN};
        }
    } else if (splitter == '-') {
        if (dir == UP || dir == DOWN) {
            return {LEFT, RIGHT};
        }
    }
    return {dir};
}

int simulate_beam(const std::vector<std::string>& grid, Point start_pos, DirectionEnum start_dir) {
    int height = grid.size();
    int width = grid[0].length();
    std::deque<BeamState> q;
    
    std::vector<std::vector<std::vector<bool>>> visited(height, std::vector<std::vector<bool>>(width, std::vector<bool>(NUM_DIRECTIONS, false)));
    std::vector<std::vector<bool>> energized(height, std::vector<bool>(width, false));

    q.push_back({start_pos, start_dir});

    while (!q.empty()) {
        BeamState current = q.front();
        q.pop_front();

        int x = current.pos.x;
        int y = current.pos.y;
        DirectionEnum dir = current.dir;

        if (visited[y][x][dir]) {
            continue;
        }
        visited[y][x][dir] = true;
        energized[y][x] = true;

        int nx = x + DX[dir];
        int ny = y + DY[dir];

        if (nx < 0 || nx >= width || ny < 0 || ny >= height) {
            continue;
        }

        char cell = grid[ny][nx];

        if (cell == '.') {
            q.push_back({{nx, ny}, dir});
        } else if (cell == '/' || cell == '\\') {
            q.push_back({{nx, ny}, reflect(dir, cell)});
        } else if (cell == '|' || cell == '-') {
            std::vector<DirectionEnum> new_dirs = split_beam(dir, cell);
            for (DirectionEnum new_dir : new_dirs) {
                q.push_back({{nx, ny}, new_dir});
            }
        }
    }

    int energized_count = 0;
    for (int r = 0; r < height; ++r) {
        for (int c = 0; c < width; ++c) {
            if (energized[r][c]) {
                energized_count++;
            }
        }
    }
    return energized_count;
}

std::vector<std::pair<Point, DirectionEnum>> get_edge_start_positions(const std::vector<std::string>& grid) {
    std::vector<std::pair<Point, DirectionEnum>> starts;
    int height = grid.size();
    int width = grid[0].length();

    for (int x = 0; x < width; ++x) {
        if (grid[0][x] != '#') starts.push_back({{x, 0}, DOWN});
        if (grid[height - 1][x] != '#') starts.push_back({{x, height - 1}, UP});
    }

    for (int y = 1; y < height - 1; ++y) {
        if (grid[y][0] != '#') starts.push_back({{0, y}, RIGHT});
        if (grid[y][width - 1] != '#') starts.push_back({{width - 1, y}, LEFT});
    }
    return starts;
}

int part_two(const std::vector<std::string>& grid) {
    std::vector<std::pair<Point, DirectionEnum>> starts = get_edge_start_positions(grid);
    int max_energized = 0;

    for (const auto& start_config : starts) {
        int energized_count = simulate_beam(grid, start_config.first, start_config.second);
        if (energized_count > max_energized) {
            max_energized = energized_count;
        }
    }
    return max_energized;
}

int main() {
    std::string input_file = "input.txt";
    std::vector<std::string> grid = read_input(input_file);

    Point part_one_start_pos = {0, 0};
    DirectionEnum part_one_start_dir = RIGHT;
    int energized_part_one = simulate_beam(grid, part_one_start_pos, part_one_start_dir);
    std::cout << energized_part_one << std::endl;

    int max_energized_part_two = part_two(grid);
    std::cout << max_energized_part_two << std::endl;

    return 0;
}

