
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <utility>
#include <stdexcept>
#include <cctype>

struct P {
    int x, y;
    bool operator==(const P& other) const { return x == other.x && y == other.y; }
    bool operator<(const P& other) const {
        if (x != other.x) return x < other.x;
        return y < other.y;
    }
};

enum class Dir : int { N = 0, E = 1, S = 2, W = 3 };

Dir rotate(Dir current, char direction) {
    if (direction == 'R') {
        return static_cast<Dir>((static_cast<int>(current) + 1) % 4);
    } else if (direction == 'L') {
        return static_cast<Dir>((static_cast<int>(current) - 1 + 4) % 4);
    }
    return current;
}

int points(Dir d) {
    return (static_cast<int>(d) + 3) % 4;
}

struct Movement {
    int steps;
    char rotate_char;
};

std::pair<P, Dir> cross_border(P n, Dir dir, int size) {
    int x = n.x;
    int y = n.y;
    int S = size;

    if (x == -1 && y < 2 * S) {
        return {P{y + 2 * S, x + 1}, Dir::E};
    } else if (x == -1 && y >= 2 * S) {
        return {P{x + 4 * S, y - 2 * S}, Dir::N};
    } else if (x == S && dir == Dir::S) {
        return {P{y - S, x + S - 1}, Dir::W};
    } else if (x == 2 * S - 1 && dir == Dir::N) {
        return {P{y + S, x - S + 1}, Dir::E};
    } else if (x == 3 * S && dir == Dir::S) {
        return {P{y + 2 * S, x - 2 * S - 1}, Dir::W};
    } else if (x == 4 * S) {
        return {P{x - 4 * S, y + 2 * S}, Dir::S};
    } else if (y == -1 && x < 3 * S) {
        return {P{3 * S - 1 - x, y + S + 1}, Dir::E};
    } else if (y == -1 && x >= 3 * S) {
        return {P{y + 1, x - 2 * S}, Dir::S};
    } else if (y == S - 1 && x < S) {
        return {P{3 * S - 1 - x, y - S + 1}, Dir::E};
    } else if (y == S - 1 && x >= S && dir == Dir::W) {
        return {P{y + S + 1, x - S}, Dir::S};
    } else if (y == S && dir == Dir::E) {
        return {P{y + 2 * S - 1, x - 2 * S}, Dir::N};
    } else if (y == 2 * S && x < 2 * S && dir == Dir::E) {
        return {P{y - S - 1, x + S}, Dir::N};
    } else if (y == 2 * S && x >= 2 * S) {
        return {P{3 * S - 1 - x, y + S - 1}, Dir::W};
    } else if (y == 3 * S) {
        return {P{3 * S - 1 - x, y - S - 1}, Dir::W};
    } else {
        throw std::runtime_error("Not a border crossing");
    }
}

struct Human {
    P curr;
    Dir facing;

    std::pair<P, Dir> walk(const std::map<P, bool>& map_data, const std::vector<P>& dirs, int size) {
        P dir_delta = dirs[static_cast<int>(facing)];
        P next_pos = P{curr.x + dir_delta.x, curr.y + dir_delta.y};

        auto it = map_data.find(next_pos);
        if (it != map_data.end()) {
            if (it->second) {
                return {curr, facing};
            } else {
                return {next_pos, facing};
            }
        } else {
            std::pair<P, Dir> new_state = cross_border(next_pos, facing, size);
            P new_pos = new_state.first;
            Dir new_facing = new_state.second;

            auto it_new = map_data.find(new_pos);
            if (it_new != map_data.end() && it_new->second) {
                return {curr, facing};
            }
            return {new_pos, new_facing};
        }
    }
};

std::vector<Movement> parse_path(const std::string& path) {
    std::vector<Movement> movements;
    int acc = 0;
    for (char char_ : path) {
        if (char_ == 'R' || char_ == 'L') {
            if (acc != 0) {
                movements.push_back(Movement{acc, '\0'});
                acc = 0;
            }
            movements.push_back(Movement{0, char_});
        } else if (isdigit(char_)) {
            acc = acc * 10 + (char_ - '0');
        }
    }
    if (acc != 0) {
        movements.push_back(Movement{acc, '\0'});
    }
    return movements;
}

struct ParsedInput {
    std::map<P, bool> map_data;
    int size;
    std::vector<Movement> movements;
};

ParsedInput parse_input(const std::string& filename) {
    std::map<P, bool> map_data;
    int size = 0;
    std::vector<Movement> movements;

    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }

    std::string line;
    std::vector<std::string> map_lines;
    bool parsing_map = true;

    while (std::getline(file, line)) {
        if (parsing_map) {
            if (line.empty()) {
                parsing_map = false;
                continue;
            }
            map_lines.push_back(line);
        } else {
            movements = parse_path(line);
            break;
        }
    }

    if (!map_lines.empty()) {
        size = map_lines[0].length() / 3;

        for (int r = 0; r < map_lines.size(); ++r) {
            const std::string& map_line = map_lines[r];
            for (int c = 0; c < map_line.length(); ++c) {
                char char_ = map_line[c];
                if (char_ == ' ') {
                    continue;
                } else if (char_ == '#') {
                    map_data[P{r, c}] = true;
                } else if (char_ == '.') {
                    map_data[P{r, c}] = false;
                }
            }
        }
    } else {
        throw std::runtime_error("No map data found in input file.");
    }

    return {map_data, size, movements};
}

int main() {
    try {
        ParsedInput parsed_data = parse_input("input.txt");
        const std::map<P, bool>& map_data = parsed_data.map_data;
        int size = parsed_data.size;
        const std::vector<Movement>& movements = parsed_data.movements;

        const std::vector<P> dirs = {
            P{-1, 0},
            P{0, 1},
            P{1, 0},
            P{0, -1}
        };

        Human human = Human{P{0, size}, Dir::E};

        for (const auto& mov : movements) {
            if (mov.rotate_char != '\0') {
                human.facing = rotate(human.facing, mov.rotate_char);
            }
            for (int i = 0; i < mov.steps; ++i) {
                std::pair<P, Dir> new_state = human.walk(map_data, dirs, size);
                P new_pos = new_state.first;
                Dir new_facing = new_state.second;

                if (new_pos == human.curr && new_facing == human.facing) {
                    break;
                }
                human.curr = new_pos;
                human.facing = new_facing;
            }
        }

        long long final_value = 1000LL * (human.curr.x + 1) + 4LL * (human.curr.y + 1) + points(human.facing);
        std::cout << final_value << std::endl;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    return 0;
}
