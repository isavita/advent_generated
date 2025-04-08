
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>

using namespace std;

struct Coord {
    int x, y;

    Coord(int x = 0, int y = 0) : x(x), y(y) {}

    Coord operator+(const Coord& c2) const {
        return Coord(x + c2.x, y + c2.y);
    }

    bool isInBounds(int width, int height) const {
        return 0 <= x && x < width && 0 <= y && y < height;
    }

    size_t hash() const {
        size_t h1 = x;
        size_t h2 = y;
        return h1 ^ (h2 << 1);
    }

    bool operator==(const Coord& other) const {
        return x == other.x && y == other.y;
    }
};

namespace std {
    template <>
    struct hash<Coord> {
        size_t operator()(const Coord& c) const {
            return c.hash();
        }
    };
}

class Grid {
public:
    int width, height;
    unordered_map<Coord, char> data;

    Grid(const vector<string>& input_data) {
        width = input_data[0].length();
        height = input_data.size();
        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                if (input_data[y][x] != '.') {
                    data[Coord(x, y)] = input_data[y][x];
                }
            }
        }
    }

    void shiftSingleRock(const Coord& coord, const Coord& direction) {
        if (data.count(coord) && data[coord] == 'O') {
            Coord current = coord;
            Coord before = coord + direction;
            while (before.isInBounds(width, height) && !data.count(before)) {
                data[before] = 'O';
                data.erase(current);
                current = before;
                before = before + direction;
            }
        }
    }

    void shiftRocks(const Coord& direction) {
        if (direction.x == 0 && direction.y == -1 || direction.x == -1 && direction.y == 0) {
            for (int x = 0; x < width; ++x) {
                for (int y = 0; y < height; ++y) {
                    shiftSingleRock(Coord(x, y), direction);
                }
            }
        } else {
            for (int x = width - 1; x >= 0; --x) {
                for (int y = height - 1; y >= 0; --y) {
                    shiftSingleRock(Coord(x, y), direction);
                }
            }
        }
    }

    void cycleRocks() {
        shiftRocks(Coord(0, -1));
        shiftRocks(Coord(-1, 0));
        shiftRocks(Coord(0, 1));
        shiftRocks(Coord(1, 0));
    }

    size_t calculateGridKey() const {
        size_t key = 0;
        for (const auto& pair : data) {
            if (pair.second == 'O') {
                key += pair.first.x + pair.first.y * width;
            }
        }
        return key;
    }

    int calculateLoad() const {
        int load = 0;
        for (const auto& pair : data) {
            if (pair.second == 'O') {
                load += height - pair.first.y;
            }
        }
        return load;
    }
};

int solve(const vector<string>& input_data) {
    int num_cycles = 1000000000;
    Grid grid(input_data);
    unordered_map<size_t, int> cache;

    for (int i = 0; i < num_cycles; ++i) {
        size_t grid_key = grid.calculateGridKey();
        if (cache.count(grid_key)) {
            int i_start_cycle = cache[grid_key];
            int cycle_length = i - i_start_cycle;
            int remaining_cycles = (num_cycles - i_start_cycle) % cycle_length;
            for (int j = 0; j < remaining_cycles; ++j) {
                grid.cycleRocks();
            }
            return grid.calculateLoad();
        }
        cache[grid_key] = i;
        grid.cycleRocks();
    }

    return grid.calculateLoad();
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    vector<string> input_data;
    string line;
    while (getline(file, line)) {
        input_data.push_back(line);
    }

    cout << solve(input_data) << endl;

    return 0;
}
