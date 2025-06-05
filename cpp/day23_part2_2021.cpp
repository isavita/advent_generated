
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <queue>
#include <algorithm>
#include <fstream>
#include <limits>

using Coord = std::pair<int, int>;

long long calc_energy(char amphipod_char, Coord start, Coord end) {
    long long dist = std::abs(end.second - start.second);
    dist += start.first - 1;
    dist += end.first - 1;

    long long energy_per_type;
    switch (amphipod_char) {
        case 'A': energy_per_type = 1; break;
        case 'B': energy_per_type = 10; break;
        case 'C': energy_per_type = 100; break;
        case 'D': energy_per_type = 1000; break;
        default: return -1; // Should not happen with valid input
    }
    return energy_per_type * dist;
}

struct State {
    std::vector<std::vector<char>> grid;
    long long energy_used;

    State(const std::vector<std::vector<char>>& g, long long eu)
        : grid(g), energy_used(eu) {}

    bool operator>(const State& other) const {
        return energy_used > other.energy_used;
    }

    bool operator<(const State& other) const {
        return grid < other.grid;
    }

    bool is_all_done(const std::map<Coord, char>& room_coord_to_want_char) const {
        for (const auto& entry : room_coord_to_want_char) {
            if (grid[entry.first.first][entry.first.second] != entry.second) {
                return false;
            }
        }
        return true;
    }

    std::vector<Coord> get_unsettled_coords(const std::map<Coord, char>& room_coord_to_want_char) const {
        std::vector<Coord> unsettled;

        for (int col = 1; col < grid[0].size(); ++col) {
            if (grid[1][col] >= 'A' && grid[1][col] <= 'D') {
                unsettled.push_back({1, col});
            }
        }

        for (int col : {3, 5, 7, 9}) {
            bool room_full_from_back = true;
            for (int row = grid.size() - 2; row > 1; --row) {
                Coord current_coord = {row, col};
                char current_char = grid[row][col];
                
                auto it = room_coord_to_want_char.find(current_coord);

                if (current_char != '.') {
                    char desired_char = (it != room_coord_to_want_char.end()) ? it->second : '\0';

                    if (it == room_coord_to_want_char.end() || current_char != desired_char) {
                        room_full_from_back = false;
                        unsettled.push_back(current_coord);
                    } else if (current_char == desired_char && !room_full_from_back) {
                        unsettled.push_back(current_coord);
                    }
                } else {
                    room_full_from_back = false;
                }
            }
        }
        return unsettled;
    }

    std::vector<Coord> get_next_possible_moves(Coord unsettled_coord, const std::map<Coord, char>& room_coord_to_want_char) const {
        std::vector<Coord> possible_destinations;
        char amphipod_char = grid[unsettled_coord.first][unsettled_coord.second];
        
        std::queue<Coord> q;
        std::set<Coord> visited;
        
        q.push(unsettled_coord);
        visited.insert(unsettled_coord);

        int dr[] = {-1, 1, 0, 0};
        int dc[] = {0, 0, -1, 1};

        bool started_in_hallway = (unsettled_coord.first == 1);

        while (!q.empty()) {
            Coord current_pos = q.front();
            q.pop();

            for (int i = 0; i < 4; ++i) {
                Coord next_pos = {current_pos.first + dr[i], current_pos.second + dc[i]};

                if (next_pos.first >= 0 && next_pos.first < grid.size() &&
                    next_pos.second >= 0 && next_pos.second < grid[0].size() &&
                    grid[next_pos.first][next_pos.second] == '.' &&
                    visited.find(next_pos) == visited.end()) {
                    
                    visited.insert(next_pos);
                    q.push(next_pos);

                    bool next_is_hallway = (next_pos.first == 1);
                    bool next_is_hallway_entrance = (next_is_hallway && (next_pos.second == 3 || next_pos.second == 5 || next_pos.second == 7 || next_pos.second == 9));

                    if (next_is_hallway_entrance) {
                        continue;
                    }

                    if (started_in_hallway) {
                        if (!next_is_hallway) {
                            auto it = room_coord_to_want_char.find(next_pos);
                            if (it != room_coord_to_want_char.end() && it->second == amphipod_char) {
                                bool room_has_deeper_open_spaces = false;
                                bool room_has_wrong_amphipod = false;
                                for (int r = next_pos.first + 1; r < grid.size() - 1; ++r) {
                                    char char_at_r_c = grid[r][next_pos.second];
                                    if (char_at_r_c == '.') {
                                        room_has_deeper_open_spaces = true;
                                        break;
                                    } else if (char_at_r_c != amphipod_char) {
                                        room_has_wrong_amphipod = true;
                                        break;
                                    }
                                }
                                if (!room_has_deeper_open_spaces && !room_has_wrong_amphipod) {
                                    possible_destinations.push_back(next_pos);
                                }
                            }
                        }
                    } else {
                        if (next_is_hallway) {
                            possible_destinations.push_back(next_pos);
                        }
                    }
                }
            }
        }
        return possible_destinations;
    }
};

long long solve_amphipod(const std::string& input_str) {
    std::vector<std::string> lines;
    std::string current_line;
    for (char c : input_str) {
        if (c == '\n') {
            lines.push_back(current_line);
            current_line.clear();
        } else {
            current_line += c;
        }
    }
    lines.push_back(current_line);

    std::vector<std::vector<char>> initial_grid(lines.size(), std::vector<char>(lines[0].length()));
    for (size_t r = 0; r < lines.size(); ++r) {
        for (size_t c = 0; c < lines[r].length(); ++c) {
            initial_grid[r][c] = lines[r][c];
        }
    }

    std::vector<std::vector<char>> part2_grid(initial_grid.size() + 2, std::vector<char>(initial_grid[0].size()));
    for(size_t c = 0; c < initial_grid[0].size(); ++c) {
        part2_grid[0][c] = initial_grid[0][c];
        part2_grid[1][c] = initial_grid[1][c];
        part2_grid[part2_grid.size() - 1][c] = initial_grid[initial_grid.size() - 1][c];
    }

    for(size_t c = 0; c < initial_grid[0].size(); ++c) {
        part2_grid[2][c] = initial_grid[2][c];
        part2_grid[5][c] = initial_grid[3][c];
        part2_grid[6][c] = initial_grid[4][c];
    }
    part2_grid[3] = {' ', ' ', '#', 'D', '#', 'C', '#', 'B', '#', 'A', '#', ' ', ' '};
    part2_grid[4] = {' ', ' ', '#', 'D', '#', 'B', '#', 'A', '#', 'C', '#', ' ', ' '};

    State start_state(part2_grid, 0);

    std::map<Coord, char> room_coord_to_want_char;
    room_coord_to_want_char[{2, 3}] = 'A'; room_coord_to_want_char[{3, 3}] = 'A'; room_coord_to_want_char[{4, 3}] = 'A'; room_coord_to_want_char[{5, 3}] = 'A';
    room_coord_to_want_char[{2, 5}] = 'B'; room_coord_to_want_char[{3, 5}] = 'B'; room_coord_to_want_char[{4, 5}] = 'B'; room_coord_to_want_char[{5, 5}] = 'B';
    room_coord_to_want_char[{2, 7}] = 'C'; room_coord_to_want_char[{3, 7}] = 'C'; room_coord_to_want_char[{4, 7}] = 'C'; room_coord_to_want_char[{5, 7}] = 'C';
    room_coord_to_want_char[{2, 9}] = 'D'; room_coord_to_want_char[{3, 9}] = 'D'; room_coord_to_want_char[{4, 9}] = 'D'; room_coord_to_want_char[{5, 9}] = 'D';

    std::priority_queue<State, std::vector<State>, std::greater<State>> min_heap;
    min_heap.push(start_state);

    std::set<std::vector<std::vector<char>>> seen_grids;

    long long min_energy = std::numeric_limits<long long>::max();

    while (!min_heap.empty()) {
        State current_state = min_heap.top();
        min_heap.pop();

        if (current_state.energy_used >= min_energy) {
            continue;
        }

        if (current_state.is_all_done(room_coord_to_want_char)) {
            min_energy = std::min(min_energy, current_state.energy_used);
            continue;
        }

        if (seen_grids.count(current_state.grid)) {
            continue;
        }
        seen_grids.insert(current_state.grid);

        std::vector<Coord> unsettled_coords = current_state.get_unsettled_coords(room_coord_to_want_char);

        for (Coord unsettled_coord : unsettled_coords) {
            std::vector<Coord> next_moves = current_state.get_next_possible_moves(unsettled_coord, room_coord_to_want_char);
            char amphipod_char = current_state.grid[unsettled_coord.first][unsettled_coord.second];

            for (Coord next_coord : next_moves) {
                State next_state = current_state;
                
                long long energy_cost = calc_energy(amphipod_char, unsettled_coord, next_coord);
                next_state.energy_used += energy_cost;

                next_state.grid[next_coord.first][next_coord.second] = amphipod_char;
                next_state.grid[unsettled_coord.first][unsettled_coord.second] = '.';

                if (next_state.energy_used < min_energy) {
                    min_heap.push(next_state);
                }
            }
        }
    }

    if (min_energy == std::numeric_limits<long long>::max()) {
        throw std::runtime_error("No solution found.");
    }
    return min_energy;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::string input_str((std::istreambuf_iterator<char>(file)),
                           std::istreambuf_iterator<char>());
    file.close();

    std::cout << solve_amphipod(input_str) << std::endl;

    return 0;
}
