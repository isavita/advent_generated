
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <queue>
#include <unordered_set>
#include <map>
#include <utility>
#include <algorithm>
#include <vector>

// Represents a generator or a microchip
struct Halves {
    int material_id; // Unique ID for the material
    bool is_chip;

    // Needed for sorting and comparison
    bool operator<(const Halves& other) const {
        if (material_id != other.material_id) {
            return material_id < other.material_id;
        }
        return is_chip < other.is_chip;
    }

    bool operator==(const Halves& other) const {
        return material_id == other.material_id && is_chip == other.is_chip;
    }
};

// Represents the state of the facility
struct State {
    std::vector<std::vector<Halves>> floors;
    int elevator_level;
    int steps;
    int num_materials; // Total number of unique materials

    // Default constructor
    State(int n_floors = 4, int n_mats = 0)
        : floors(n_floors), elevator_level(0), steps(0), num_materials(n_mats) {}

    // Copy constructor (default is sufficient)
    // State(const State&) = default;
    // State& operator=(const State&) = default;


    // Generates a canonical string representation for hashing/visited set
    std::string hash_key() const {
        std::vector<int> gen_floor(num_materials, -1);
        std::vector<int> chip_floor(num_materials, -1);

        for (int fl_index = 0; fl_index < floors.size(); ++fl_index) {
            for (const auto& half : floors[fl_index]) {
                if (half.is_chip) {
                    chip_floor[half.material_id] = fl_index;
                } else {
                    gen_floor[half.material_id] = fl_index;
                }
            }
        }

        std::vector<std::pair<int, int>> pairs;
        pairs.reserve(num_materials);
        for (int i = 0; i < num_materials; ++i) {
            pairs.emplace_back(gen_floor[i], chip_floor[i]);
        }
        std::sort(pairs.begin(), pairs.end());

        std::stringstream ss;
        ss << elevator_level << "|";
        for (const auto& p : pairs) {
            ss << p.first << "," << p.second << ";";
        }
        return ss.str();
    }

    // Checks if the current state is valid (no chip fried)
    bool is_valid() const {
        for (const auto& floor : floors) {
            bool has_generator = false;
            std::vector<int> chip_materials;
            std::unordered_set<int> gen_materials;

            for (const auto& half : floor) {
                if (half.is_chip) {
                    chip_materials.push_back(half.material_id);
                } else {
                    has_generator = true;
                    gen_materials.insert(half.material_id);
                }
            }

            if (!has_generator || chip_materials.empty()) {
                continue; // No generators or no chips on this floor, safe
            }

            for (int chip_mat_id : chip_materials) {
                // If a chip's generator is not present, but other generators are
                if (gen_materials.find(chip_mat_id) == gen_materials.end()) {
                    return false; // Fried chip!
                }
            }
        }
        return true;
    }

    // Checks if all items are on the top floor
    bool is_done() const {
        for (size_t i = 0; i < floors.size() - 1; ++i) {
            if (!floors[i].empty()) {
                return false;
            }
        }
        return true;
    }

    // Generates all valid next states reachable in one step
    std::vector<State> get_next_states() const {
        std::vector<State> future_states;
        const auto& current_floor_items = floors[elevator_level];
        int n_items = current_floor_items.size();

        // Possible elevator moves (-1 for down, +1 for up)
        std::vector<int> ele_diffs;
        if (elevator_level > 0) ele_diffs.push_back(-1);
        if (elevator_level < floors.size() - 1) ele_diffs.push_back(1);

        // Iterate through possible numbers of items to move (1 or 2)
        for (int r = 1; r <= 2; ++r) {
            // Generate combinations of indices
            std::vector<int> combo_indices(r);
            std::vector<bool> v(n_items);
            std::fill(v.begin() + n_items - r, v.end(), true);

            do {
                std::vector<int> current_combo_indices;
                 std::vector<Halves> items_to_move;
                for (int i = 0; i < n_items; ++i) {
                    if (v[i]) {
                        current_combo_indices.push_back(i);
                        items_to_move.push_back(current_floor_items[i]);
                    }
                }

                 // Try moving these items up or down
                for (int ele_diff : ele_diffs) {
                    State next_state = *this; // Make a copy
                    next_state.elevator_level += ele_diff;
                    next_state.steps += 1;
                    int old_level = elevator_level;
                    int new_level = next_state.elevator_level;

                    // Add items to the new floor
                     for(const auto& item : items_to_move) {
                        next_state.floors[new_level].push_back(item);
                     }
                    // Sort new floor for potential canonical representation (optional but good practice)
                    // std::sort(next_state.floors[new_level].begin(), next_state.floors[new_level].end());

                    // Remove items from the old floor efficiently
                    // Create a temporary vector excluding the moved items
                    std::vector<Halves> updated_old_floor;
                    updated_old_floor.reserve(n_items - r);
                    int moved_idx = 0;
                    for(int i=0; i<n_items; ++i) {
                        bool moved = false;
                        if (moved_idx < current_combo_indices.size() && i == current_combo_indices[moved_idx]) {
                             moved = true;
                             moved_idx++;
                        }
                        if (!moved) {
                            updated_old_floor.push_back(current_floor_items[i]);
                        }
                    }
                    next_state.floors[old_level] = std::move(updated_old_floor);


                    if (next_state.is_valid()) {
                        future_states.push_back(std::move(next_state));
                    }
                }
            } while (std::next_permutation(v.begin(), v.end()));
        }

        return future_states;
    }
};


int main() {
    std::ifstream infile("input.txt");
    if (!infile) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::string line;
    std::vector<std::vector<Halves>> initial_floors(4);
    std::map<std::string, int> material_to_id;
    int next_material_id = 0;
    int line_index = 0;

    while (std::getline(infile, line)) {
        std::stringstream ss(line);
        std::string word;
        std::string prev_word;
        bool contains_relevant = false;

        while (ss >> word) {
             // Clean up punctuation
             if (!word.empty() && (word.back() == ',' || word.back() == '.')) {
                 word.pop_back();
             }

            std::string material_name;
            bool is_chip = false;

            if (word == "generator") {
                 material_name = prev_word;
                 is_chip = false;
                 contains_relevant = true;
            } else if (word == "microchip") {
                 size_t hyphen_pos = prev_word.find("-compatible");
                 if(hyphen_pos != std::string::npos) {
                    material_name = prev_word.substr(0, hyphen_pos);
                 } else {
                     material_name = prev_word; // Should not happen based on format but safer
                 }

                 is_chip = true;
                 contains_relevant = true;
            }

            if(contains_relevant) {
                 if (material_to_id.find(material_name) == material_to_id.end()) {
                    material_to_id[material_name] = next_material_id++;
                 }
                 initial_floors[line_index].push_back({material_to_id[material_name], is_chip});
                 contains_relevant = false; // Reset for next item on line
            }
            prev_word = word;
        }
        line_index++;
    }
    infile.close();

    // Add items for Part 2
    std::vector<std::pair<std::string, bool>> part2_items = {
        {"elerium", false}, {"elerium", true},
        {"dilithium", false}, {"dilithium", true}
    };

     for(const auto& p : part2_items) {
        const std::string& material_name = p.first;
        bool is_chip = p.second;
         if (material_to_id.find(material_name) == material_to_id.end()) {
            material_to_id[material_name] = next_material_id++;
         }
         initial_floors[0].push_back({material_to_id[material_name], is_chip});
     }


    State initial_state(4, next_material_id);
    initial_state.floors = std::move(initial_floors);
    // Optional: Sort initial floors if needed later, hash_key doesn't strictly require it
    // for(auto& floor : initial_state.floors) {
    //     std::sort(floor.begin(), floor.end());
    // }


    std::queue<State> q;
    std::unordered_set<std::string> visited;

    q.push(initial_state);
    visited.insert(initial_state.hash_key());

    int final_steps = -1;

    while (!q.empty()) {
        State current = std::move(q.front());
        q.pop();

        if (current.is_done()) {
            final_steps = current.steps;
            break;
        }

        std::vector<State> next_states = current.get_next_states();
        for (State& next : next_states) {
            std::string key = next.hash_key();
            if (visited.find(key) == visited.end()) {
                visited.insert(key);
                q.push(std::move(next)); // Move ownership to queue
            }
        }
    }

    std::cout << final_steps << std::endl;

    return 0;
}
