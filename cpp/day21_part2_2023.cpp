
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_set>
#include <stdexcept>

struct Point {
    int x, y;
    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }
    Point operator+(const Point& other) const {
        return {x + other.x, y + other.y};
    }
};

namespace std {
    template <> struct hash<Point> {
        size_t operator()(const Point& p) const {
            return std::hash<int>()(p.x) ^ (std::hash<int>()(p.y) << 1);
        }
    };
}

Point mod_point(Point p, int mod) {
    return {((p.x % mod) + mod) % mod,
            ((p.y % mod) + mod) % mod};
}

void parse_data(const std::vector<std::string>& data,
                std::unordered_set<Point>& garden, Point& start, int& max_size) {
    max_size = data.size();
    if (max_size == 0) {
        throw std::runtime_error("Empty input data.");
    }

    for (int y = 0; y < max_size; ++y) {
        if (data[y].length() != max_size) {
            throw std::runtime_error("Input grid is not square.");
        }
        for (int x = 0; x < max_size; ++x) {
            char c = data[y][x];
            if (c != '#') {
                garden.insert({x, y});
            }
            if (c == 'S') {
                start = {x, y};
            }
        }
    }
    if (garden.find(start) == garden.end()) {
        throw std::runtime_error("Start position 'S' not found or is a wall.");
    }
}

long long quadratic_function(long long n, long long a, long long b, long long c) {
    return a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2));
}

long long calculate_num_ends(const std::unordered_set<Point>& garden, Point start,
                             long long num_iterations, int max_size) {
    std::unordered_set<Point> current_reachable;
    current_reachable.insert(start);

    std::vector<long long> done_counts;
    const int target_offset = (max_size - 1) / 2;

    const std::vector<Point> dirs = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

    for (int i = 0; i < 3 * max_size; ++i) {
        if ((i % max_size) == target_offset) {
            done_counts.push_back(current_reachable.size());
        }
        if (done_counts.size() == 3) {
            break;
        }

        std::unordered_set<Point> next_reachable;
        for (const auto& p : current_reachable) {
            for (const auto& dir : dirs) {
                Point new_p = p + dir;
                if (garden.count(mod_point(new_p, max_size))) {
                    next_reachable.insert(new_p);
                }
            }
        }
        current_reachable = next_reachable;
    }

    if (done_counts.size() < 3) {
        throw std::runtime_error("Not enough data points collected for quadratic extrapolation.");
    }

    long long n_val = num_iterations / max_size;

    return quadratic_function(n_val, done_counts[0], done_counts[1], done_counts[2]);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::vector<std::string> garden_input;
    std::string line;
    while (std::getline(file, line)) {
        garden_input.push_back(line);
    }
    file.close();

    std::unordered_set<Point> garden;
    Point start = {0, 0};
    int max_size = 0;

    try {
        parse_data(garden_input, garden, start, max_size);
        long long num_iterations = 26501365;
        long long result = calculate_num_ends(garden, start, num_iterations, max_size);
        std::cout << result << std::endl;
    } catch (const std::runtime_error& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
