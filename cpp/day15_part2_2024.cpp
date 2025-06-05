
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <sstream>
#include <numeric>
#include <fstream>
#include <algorithm> // For std::remove

// Represents a 2D point (x, y)
struct Point {
    int x, y;

    Point(int x_ = 0, int y_ = 0) : x(x_), y(y_) {}

    // Overload + operator for point addition
    Point operator+(const Point& other) const {
        return Point(x + other.x, y + other.y);
    }

    // Overload * operator for scalar multiplication (e.g., 2 * Point(1,0))
    Point operator*(int scalar) const {
        return Point(x * scalar, y * scalar);
    }

    // Overload == operator for point comparison
    bool operator==(const Point& other) const {
        return x == other.x && y == other.y;
    }

    // Overload < operator for use with std::map (required for sorting)
    bool operator<(const Point& other) const {
        if (y != other.y) return y < other.y;
        return x < other.x;
    }
};

// Helper function to get character from map, treating unknown positions as '.'
char get_char(const std::map<Point, char>& m, const Point& p) {
    auto it = m.find(p);
    if (it != m.end()) {
        return it->second;
    }
    return '.'; // Default for out-of-bounds or undefined map entries
}

// Recursive helper function to simulate a step, handling object pushes
bool try_to_step(std::map<Point, char>& m, Point pos, Point dir) {
    std::map<Point, char> original_m = m; // Save current map state for rollback

    char char_at_pos = get_char(m, pos);

    bool success = false;
    if (char_at_pos == '.') {
        success = true; // Can step into an empty space
    } else if (char_at_pos == 'O' || char_at_pos == '@') {
        // Pushable objects ('O' or '@')
        if (try_to_step(m, pos + dir, dir)) {
            m[pos + dir] = char_at_pos; // Move object to next spot
            m[pos] = '.';               // Current spot becomes empty
            success = true;
        }
    } else if (char_at_pos == ']') {
        // Right part of a box. Delegate push to the left part.
        if (try_to_step(m, pos + Point(-1, 0), dir)) {
            success = true;
        }
    } else if (char_at_pos == '[') {
        // Left part of a box
        Point right_part = pos + Point(1, 0);
        char char_at_right = get_char(m, right_part);

        if (char_at_right == ']') { // Ensure it's a valid box '[]'
            if (dir == Point(-1, 0)) { // Pushing left
                if (try_to_step(m, pos + Point(-1, 0), dir)) {
                    m[pos + Point(-1, 0)] = '['; // Move '['
                    m[pos] = ']';               // Move ']'
                    m[right_part] = '.';        // Old ']' location becomes '.'
                    success = true;
                }
            } else if (dir == Point(1, 0)) { // Pushing right
                if (try_to_step(m, pos + Point(2, 0), dir)) {
                    m[pos] = '.';               // Old '[' location becomes '.'
                    m[pos + Point(1, 0)] = '['; // Move '['
                    m[pos + Point(2, 0)] = ']'; // Move ']'
                    success = true;
                }
            } else { // Pushing up or down
                if (try_to_step(m, pos + dir, dir) && try_to_step(m, right_part + dir, dir)) {
                    m[pos] = '.';             // Old '[' becomes '.'
                    m[right_part] = '.';      // Old ']' becomes '.'
                    m[pos + dir] = '[';       // Move '['
                    m[right_part + dir] = ']'; // Move ']'
                    success = true;
                }
            }
        }
    }

    if (!success) {
        m = original_m; // Rollback map to original state if move failed
    }
    return success;
}

// Solves the puzzle for a given input string
long long solve(const std::string& input_str) {
    // Mimic Python's `input_str.strip().split("\n\n")`
    size_t first_double_newline = input_str.find("\n\n");
    std::string map_block_str = input_str.substr(0, first_double_newline);
    std::string steps_block_str = input_str.substr(first_double_newline + 2);

    // Mimic Python's `steps_block_str.replace("\n", "")`
    steps_block_str.erase(std::remove(steps_block_str.begin(), steps_block_str.end(), '\n'), steps_block_str.end());

    std::map<Point, char> m;
    Point robot_pos;

    std::istringstream map_ss(map_block_str);
    std::string line;
    int y = 0;
    while (std::getline(map_ss, line)) {
        for (int x = 0; x < line.length(); ++x) {
            char c = line[x];
            m[Point(x, y)] = c; // Store all characters from input map, including '.'
            if (c == '@') {
                robot_pos = Point(x, y);
            }
        }
        y++;
    }

    std::vector<Point> steps;
    for (char c : steps_block_str) {
        if (c == '^') steps.push_back(Point(0, -1));
        else if (c == '<') steps.push_back(Point(-1, 0));
        else if (c == '>') steps.push_back(Point(1, 0));
        else if (c == 'v') steps.push_back(Point(0, 1));
    }

    for (const auto& dir : steps) {
        if (try_to_step(m, robot_pos, dir)) {
            robot_pos = robot_pos + dir;
        }
    }

    long long total_sum = 0;
    for (const auto& pair : m) {
        if (pair.second == '[' || pair.second == 'O') {
            total_sum += pair.first.x + 100LL * pair.first.y;
        }
    }
    return total_sum;
}

// Scales up the input string by character replacement rules
std::string scale_up(const std::string& input_str) {
    std::string result_str;
    result_str.reserve(input_str.length() * 2); // Pre-allocate memory

    for (char c : input_str) {
        if (c == '#') {
            result_str += "##";
        } else if (c == '.') {
            result_str += "..";
        } else if (c == 'O') {
            result_str += "[]";
        } else if (c == '@') {
            result_str += "@.";
        } else {
            result_str += c; // Includes newlines and other characters
        }
    }
    return result_str;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream ifs("input.txt");
    if (!ifs.is_open()) {
        std::cerr << "Error opening input.txt\n";
        return 1;
    }

    // Read entire file into a string
    std::string input_str((std::istreambuf_iterator<char>(ifs)),
                           std::istreambuf_iterator<char>());
    ifs.close();

    // Mimic Python's `strip()` to remove leading/trailing whitespace including newlines
    size_t first = input_str.find_first_not_of(" \t\n\r\f\v");
    if (std::string::npos == first) {
        input_str = ""; // String contains only whitespace
    } else {
        size_t last = input_str.find_last_not_of(" \t\n\r\f\v");
        input_str = input_str.substr(first, (last - first + 1));
    }

    std::cout << solve(input_str) << "\n";
    std::cout << solve(scale_up(input_str)) << "\n";

    return 0;
}
