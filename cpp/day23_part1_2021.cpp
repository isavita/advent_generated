
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <queue>
#include <utility> // for std::pair
#include <algorithm> // for std::swap
#include <cmath> // for std::abs
#include <sstream> // for std::stringstream
#include <fstream> // for std::ifstream

// Coordinate type
using Coord = std::pair<int, int>;
using Grid = std::vector<std::string>;

// Global maps (initialized once to avoid re-creation)
std::map<Coord, char> room_coord_to_want_char;
std::map<char, int> energy_per_type_map;

// Function declarations
int calc_energy(char amphipod_char, Coord start, Coord end);
class State; // Forward declaration

// State represents the current configuration of the grid and energy used
struct State {
    Grid grid;
    int energy_used;

    State(Grid g, int energy) : grid(std::move(g)), energy_used(energy) {}

    // Operator for std::priority_queue (min-heap behavior with std::greater)
    bool operator>(const State& other) const {
        return energy_used > other.energy_used;
    }

    // Converts grid to string for std::set for seen_grids check
    std::string toString() const {
        std::string s = "";
        for (const auto& row : grid) {
            s += row;
        }
        return s;
    }

    // Checks if all amphipods are in their final, correct positions
    bool all_done() const {
        for (const auto& entry : room_coord_to_want_char) {
            if (grid[entry.first.first][entry.first.second] != entry.second) {
                return false;
            }
        }
        return true;
    }

    // Identifies coordinates of amphipods that are not settled (either in hallway or misplaced in rooms)
    std::vector<Coord> get_unsettled_coords() const {
        std::vector<Coord> unsettled;

        // Check hallway for amphipods
        for (int c = 1; c < grid[1].length(); ++c) {
            if (grid[1][c] >= 'A' && grid[1][c] <= 'D') {
                unsettled.push_back({1, c});
            }
        }

        // Determine maximum room depth dynamically
        int max_room_row = grid.size() - 2;

        // Check rooms for misplaced or blocking amphipods
        for (int col : {3, 5, 7, 9}) { // Columns for rooms A, B, C, D
            bool room_is_perfectly_sorted = true; // True if all deeper amphipods are correct
            for (int row = max_room_row; row >= 2; --row) { // Iterate from deepest to shallowest
                char got_char = grid[row][col];
                
                // Determine the target character for this room column
                char want_char = ' ';
                if (col == 3) want_char = 'A';
                else if (col == 5) want_char = 'B';
                else if (col == 7) want_char = 'C';
                else if (col == 9) want_char = 'D';

                if (got_char != '.') { // If there's an amphipod
                    if (got_char != want_char) { // It's the wrong type
                        room_is_perfectly_sorted = false;
                        unsettled.push_back({row, col});
                    } else if (got_char == want_char && !room_is_perfectly_sorted) {
                        // It's the correct type, but an incorrect amphipod is deeper in the room
                        unsettled.push_back({row, col});
                    }
                }
            }
        }
        return unsettled;
    }

    // Finds all valid next moves for a given unsettled amphipod
    std::vector<Coord> get_next_possible_moves(Coord unsettled_coord) const {
        char amphipod_char = grid[unsettled_coord.first][unsettled_coord.second];
        if (!(amphipod_char >= 'A' && amphipod_char <= 'D')) {
            // Should not happen if get_unsettled_coords is accurate
            return {};
        }

        std::vector<Coord> possible_moves;
        std::queue<Coord> q;
        std::set<Coord> visited;

        q.push(unsettled_coord);
        visited.insert(unsettled_coord);

        bool started_in_hallway = (unsettled_coord.first == 1);

        int dr[] = {-1, 1, 0, 0}; // Row offsets: Up, Down, Left, Right
        int dc[] = {0, 0, -1, 1}; // Column offsets: Up, Down, Left, Right

        while (!q.empty()) {
            Coord current = q.front();
            q.pop();

            // Do not consider the starting position as a possible target move
            if (current != unsettled_coord) {
                bool is_hallway_spot = (current.first == 1);
                bool is_room_spot = (current.first >= 2);

                if (is_hallway_spot) {
                    // Amphipods cannot stop in the hallway directly above a room entrance
                    if (current.second != 3 && current.second != 5 && current.second != 7 && current.second != 9) {
                        if (!started_in_hallway) { // Only add if the amphipod is moving from a room to the hallway
                             possible_moves.push_back(current);
                        }
                    }
                } else if (is_room_spot) { // Must be a spot within a room
                    // Amphipods can only move into their designated room
                    char target_room_char = ' ';
                    if (current.second == 3) target_room_char = 'A';
                    else if (current.second == 5) target_room_char = 'B';
                    else if (current.second == 7) target_room_char = 'C';
                    else if (current.second == 9) target_room_char = 'D';

                    if (amphipod_char == target_room_char) { // Check if it's the correct room for this amphipod
                        // Check if all spots deeper in the room are either empty or contain correct amphipods
                        bool can_enter_room = true;
                        int max_room_row = grid.size() - 2;
                        for (int r = max_room_row; r > current.first; --r) { // Check spots strictly deeper
                            if (grid[r][current.second] != '.' && grid[r][current.second] != amphipod_char) {
                                can_enter_room = false; // Found an incorrect amphipod deeper in the room
                                break;
                            }
                        }

                        if (can_enter_room) {
                            // Also ensure it's the *deepest available* spot in the room (i.e., the spot below it is not empty)
                            bool is_deepest_available = true;
                            if (current.first < max_room_row) { // If not already at the very bottom of the room
                                if (grid[current.first + 1][current.second] == '.') { // And the spot directly below is empty
                                     is_deepest_available = false; // Then this is not the deepest available spot
                                }
                            }
                            if(is_deepest_available){
                                possible_moves.push_back(current);
                            }
                        }
                    }
                }
            }

            // Explore neighbors
            for (int i = 0; i < 4; ++i) {
                int next_row = current.first + dr[i];
                int next_col = current.second + dc[i];
                Coord next_coord = {next_row, next_col};

                // Check bounds, if the path is clear ('.'), and if not yet visited
                if (next_row >= 0 && next_row < grid.size() &&
                    next_col >= 0 && next_col < grid[0].length() &&
                    grid[next_row][next_col] == '.' &&
                    visited.find(next_coord) == visited.end())
                {
                    visited.insert(next_coord);
                    q.push(next_coord);
                }
            }
        }
        return possible_moves;
    }
};

// Parses the input string into the initial grid state
State parse_input(const std::string& input_str) {
    Grid grid;
    std::string line;
    std::stringstream ss(input_str);
    while (std::getline(ss, line)) {
        grid.push_back(line);
    }
    return State(grid, 0);
}

// Calculates the energy cost for moving an amphipod from start to end
int calc_energy(char amphipod_char, Coord start, Coord end) {
    int dist = std::abs(end.second - start.second); // Horizontal distance (hallway movement)
    dist += (start.first - 1); // Vertical distance from start room to hallway (row 1)
    dist += (end.first - 1);   // Vertical distance from hallway (row 1) to end room

    return energy_per_type_map.at(amphipod_char) * dist;
}

// Main solver function using Dijkstra-like approach
int amphipod_solver(const std::string& input_str) {
    // Initialize global maps for target room coordinates and energy costs
    // These are initialized only once
    if (room_coord_to_want_char.empty()) {
        room_coord_to_want_char[{2, 3}] = 'A'; room_coord_to_want_char[{3, 3}] = 'A';
        room_coord_to_want_char[{2, 5}] = 'B'; room_coord_to_want_char[{3, 5}] = 'B';
        room_coord_to_want_char[{2, 7}] = 'C'; room_coord_to_want_char[{3, 7}] = 'C';
        room_coord_to_want_char[{2, 9}] = 'D'; room_coord_to_want_char[{3, 9}] = 'D';
    }
    if (energy_per_type_map.empty()) {
        energy_per_type_map['A'] = 1;
        energy_per_type_map['B'] = 10;
        energy_per_type_map['C'] = 100;
        energy_per_type_map['D'] = 1000;
    }

    State start_state = parse_input(input_str);

    // Min-priority queue to explore states by lowest energy_used first
    std::priority_queue<State, std::vector<State>, std::greater<State>> min_heap;
    min_heap.push(start_state);

    // Set to keep track of visited grid states to avoid redundant computations
    std::set<std::string> seen_grids;

    while (!min_heap.empty()) {
        State current_state = min_heap.top();
        min_heap.pop();

        std::string current_grid_key = current_state.toString();
        if (seen_grids.count(current_grid_key)) {
            continue; // Already processed this grid state with lower or equal energy
        }
        seen_grids.insert(current_grid_key);

        if (current_state.all_done()) {
            return current_state.energy_used; // Found the minimum energy solution
        }

        // Get all amphipods that need to move
        std::vector<Coord> unsettled_coords = current_state.get_unsettled_coords();
        for (Coord unsettled_coord : unsettled_coords) {
            // Find all possible next positions for the current unsettled amphipod
            std::vector<Coord> next_moves = current_state.get_next_possible_moves(unsettled_coord);
            for (Coord next_coord : next_moves) {
                // Create a new state by copying the current one
                State next_state = current_state; 
                
                // Add energy cost for this specific move
                next_state.energy_used += calc_energy(next_state.grid[unsettled_coord.first][unsettled_coord.second], unsettled_coord, next_coord);
                
                // Perform the amphipod's move on the new state's grid
                std::swap(next_state.grid[unsettled_coord.first][unsettled_coord.second], next_state.grid[next_coord.first][next_coord.second]);
                
                // Push the new state to the priority queue
                min_heap.push(next_state);
            }
        }
    }
    return -1; // Should not be reached if a solution is guaranteed
}

int main() {
    std::string input_str;
    std::string line;
    std::ifstream infile("input.txt"); // Open input file

    if (!infile.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    // Read the entire file content into a string
    while (std::getline(infile, line)) {
        input_str += line;
        input_str += '\n'; // Restore newlines that getline consumes
    }
    infile.close();

    // Remove any trailing newline character to match Python's .strip() behavior
    if (!input_str.empty() && input_str.back() == '\n') {
        input_str.pop_back();
    }

    std::cout << amphipod_solver(input_str) << std::endl;

    return 0;
}
