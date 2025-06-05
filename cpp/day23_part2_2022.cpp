
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <set>
#include <map>
#include <utility>

struct Point {
    int r, c;

    bool operator<(const Point& other) const {
        if (r != other.r) {
            return r < other.r;
        }
        return c < other.c;
    }

    bool operator==(const Point& other) const {
        return r == other.r && c == other.c;
    }
};

struct Elf {
    Point pos;
    bool moving;
    Point next_pos;

    Elf(int r, int c) : pos({r, c}), moving(false), next_pos({0, 0}) {}

    bool around_all_empty(const std::set<Point>& map, const std::vector<Point>& dirs) const {
        for (const auto& d : dirs) {
            Point adj = {pos.r + d.r, pos.c + d.c};
            if (map.count(adj)) {
                return false;
            }
        }
        return true;
    }

    bool elf_in_direction(int wanna_go_dir_idx, const std::set<Point>& map, const std::vector<Point>& dirs) const {
        for (int j = -1; j <= 1; ++j) {
            int dir_idx = (wanna_go_dir_idx + j + 8) % 8;
            Point dxy = dirs[dir_idx];
            Point adj = {pos.r + dxy.r, pos.c + dxy.c};
            if (map.count(adj)) {
                return true;
            }
        }
        return false;
    }
};

std::pair<std::vector<Elf>, std::set<Point>> parse() {
    std::vector<Elf> elves;
    std::set<Point> map;
    std::ifstream file("input.txt");
    std::string line;
    int r = 0;
    while (std::getline(file, line)) {
        for (int c = 0; c < line.length(); ++c) {
            if (line[c] == '#') {
                Point p = {r, c};
                map.insert(p);
                elves.emplace_back(r, c);
            }
        }
        r++;
    }
    return {elves, map};
}

bool run(std::vector<Elf>& elves, std::set<Point>& map, const std::vector<int>& order, int curr_dir_idx, const std::vector<Point>& dirs) {
    std::map<Point, int> proposes;

    for (auto& e : elves) {
        e.moving = false;
    }

    for (auto& e : elves) {
        if (e.around_all_empty(map, dirs)) {
            continue;
        }

        bool proposed = false;
        for (int i = 0; i < 4; ++i) {
            int dir_to_check_idx = order[(curr_dir_idx + i) % 4];
            
            if (e.elf_in_direction(dir_to_check_idx, map, dirs)) {
                continue;
            }

            Point dxy = dirs[dir_to_check_idx];
            Point dest = {e.pos.r + dxy.r, e.pos.c + dxy.c};
            
            proposes[dest]++;
            e.next_pos = dest;
            e.moving = true;
            proposed = true;
            break;
        }
    }

    bool someone_moved = false;
    for (auto& e : elves) {
        if (!e.moving) {
            continue;
        }

        if (proposes[e.next_pos] > 1) {
            e.moving = false;
            continue;
        }
        
        someone_moved = true;
        map.erase(e.pos);
        map.insert(e.next_pos);
        e.pos = e.next_pos;
        e.moving = false;
    }

    return someone_moved;
}

int main() {
    const std::vector<Point> dirs = {
        {-1, -1}, {-1, 0}, {-1, 1},
        {0, 1}, {1, 1}, {1, 0},
        {1, -1}, {0, -1}
    };

    const std::vector<int> order = {1, 5, 7, 3};

    int curr_dir_idx = 0;

    auto parsed_data = parse();
    std::vector<Elf> elves = parsed_data.first;
    std::set<Point> map = parsed_data.second;

    int i = 0;
    while (true) {
        if (!run(elves, map, order, curr_dir_idx, dirs)) {
            std::cout << i + 1 << std::endl;
            break;
        }
        curr_dir_idx = (curr_dir_idx + 1) % 4;
        i++;
    }

    return 0;
}
