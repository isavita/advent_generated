
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <queue>
#include <algorithm>
#include <regex>
#include <sstream> // For std::stringstream
#include <limits>  // For std::numeric_limits

// Struct to hold valve data
struct Valve {
    std::string name;
    int flow_rate;
    std::vector<int> tunnels_to; // Original IDs of connected valves
};

// Global data structures
std::map<std::string, int> valve_name_to_id; // Maps valve names (e.g., "AA") to unique integer IDs
std::vector<Valve> all_valves; // Stores all valve data, indexed by their original integer ID

// Data structures for "important" valves (those with flow_rate > 0 and the starting valve AA)
std::vector<int> important_valve_orig_ids;         // Maps important_idx -> original_id
std::map<int, int> original_id_to_important_idx; // Maps original_id -> important_idx
std::vector<int> important_flow_rates;             // Flow rates for important_idx
int K_important_valves;                           // Total number of important valves

// dist_matrix[i][j] = shortest time to travel from important_valve[i] to important_valve[j]
std::vector<std::vector<int>> dist_matrix;

// Memoization table for Part 1 DFS
// memo[u_important_idx][minutes_left][opened_mask] stores max additional pressure from this state
// Max K_important_valves is typically around 16 (15 useful + AA).
// Max minutes_left for Part 1 is 30. Max opened_mask is (1 << 16) - 1.
// Initialized to -1 to indicate not computed.
int memo[16][31][1 << 16]; 

// Memoization table for Part 2 DFS
// memo_part2[u_important_idx][minutes_left][opened_mask] stores max total pressure accumulated to reach this state
// Max minutes_left for Part 2 is 26.
int memo_part2[16][27][1 << 16]; 

// Global map to store maximum pressure achieved for each specific opened_mask by ONE agent (for Part 2)
// This map is populated by dfs_part2.
std::map<int, int> max_pressure_for_mask_achieved;

// Part 1 DFS function: Finds the maximum additional pressure released from a given state.
// u_important_idx: Current valve's index in the `important_valve_orig_ids` list.
// minutes_left: Remaining time.
// opened_mask: Bitmask of already opened important valves.
int dfs_part1(int u_important_idx, int minutes_left, int opened_mask) {
    // Base case: No time left.
    if (minutes_left <= 0) {
        return 0;
    }
    // Memoization check
    if (memo[u_important_idx][minutes_left][opened_mask] != -1) {
        return memo[u_important_idx][minutes_left][opened_mask];
    }

    int max_pressure_released = 0; // Initialize with 0 (doing nothing useful)

    // Option 1: Open current valve u_important_idx
    // Only if it's not already open and has a positive flow rate.
    if (!(opened_mask & (1 << u_important_idx)) && important_flow_rates[u_important_idx] > 0) {
        int time_after_open = minutes_left - 1; // 1 minute to open the valve
        if (time_after_open >= 0) { // Check if we have enough time to open it
            int pressure_from_this_valve = important_flow_rates[u_important_idx] * time_after_open;
            // Recursively explore options from the same valve, with less time and this valve now open.
            max_pressure_released = std::max(max_pressure_released, 
                                             pressure_from_this_valve + dfs_part1(u_important_idx, time_after_open, opened_mask | (1 << u_important_idx)));
        }
    }

    // Option 2: Move to another important valve v_important_idx
    // This implies we are NOT opening the current valve u_important_idx at this turn.
    for (int v_important_idx = 0; v_important_idx < K_important_valves; ++v_important_idx) {
        if (u_important_idx == v_important_idx) continue; // Cannot move to the same valve

        int travel_cost = dist_matrix[u_important_idx][v_important_idx];
        int new_minutes_left = minutes_left - travel_cost;
        
        if (new_minutes_left >= 0) { // Enough time to reach the next valve
            // Recursively explore options from the new valve v_important_idx, with less time.
            // The `opened_mask` remains the same as we haven't opened any new valve yet.
            max_pressure_released = std::max(max_pressure_released, 
                                             dfs_part1(v_important_idx, new_minutes_left, opened_mask));
        }
    }
    
    // Store and return the computed maximum pressure for this state
    return memo[u_important_idx][minutes_left][opened_mask] = max_pressure_released;
}

// Part 2 DFS function: Populates `max_pressure_for_mask_achieved` with total pressure for each mask.
// u_important_idx: Current valve's index.
// minutes_left: Remaining time.
// opened_mask: Bitmask of already opened important valves.
// current_pressure_sum: Total pressure accumulated so far by valves in `opened_mask`.
void dfs_part2(int u_important_idx, int minutes_left, int opened_mask, int current_pressure_sum) {
    // Update the global map: `max_pressure_for_mask_achieved[opened_mask]` stores the highest
    // `current_pressure_sum` achieved for this `opened_mask` by any path.
    max_pressure_for_mask_achieved[opened_mask] = 
        std::max(max_pressure_for_mask_achieved[opened_mask], current_pressure_sum);

    // Memoization check for the current search state (u, time, mask):
    // If we've reached this state before with a `current_pressure_sum` that was greater than or equal,
    // then this path won't lead to a better overall result, so we prune.
    if (memo_part2[u_important_idx][minutes_left][opened_mask] >= current_pressure_sum) {
        return; 
    }
    memo_part2[u_important_idx][minutes_left][opened_mask] = current_pressure_sum;

    // Option 1: Open current valve u_important_idx
    // Only if it's not already open and has a positive flow rate.
    if (!(opened_mask & (1 << u_important_idx)) && important_flow_rates[u_important_idx] > 0) {
        int time_to_open = 1;
        int new_minutes_left = minutes_left - time_to_open;
        if (new_minutes_left >= 0) {
            int pressure_gain = important_flow_rates[u_important_idx] * new_minutes_left;
            // Recurse with less time, updated mask, and increased total pressure.
            dfs_part2(u_important_idx, new_minutes_left, opened_mask | (1 << u_important_idx), current_pressure_sum + pressure_gain);
        }
    }

    // Option 2: Move to another important valve v_important_idx
    for (int v_important_idx = 0; v_important_idx < K_important_valves; ++v_important_idx) {
        if (u_important_idx == v_important_idx) continue; // Cannot move to the same valve

        int travel_cost = dist_matrix[u_important_idx][v_important_idx];
        int new_minutes_left = minutes_left - travel_cost;
        
        if (new_minutes_left >= 0) {
            // Recurse with less time, same mask, and same current total pressure.
            dfs_part2(v_important_idx, new_minutes_left, opened_mask, current_pressure_sum);
        }
    }
}


void solve() {
    std::string line;
    int next_id = 0; // Assign unique integer IDs to valves

    // Regex to parse input lines: "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    std::regex valve_regex("Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? ([A-Z, ]+)");

    // Phase 1: Parse input and build initial graph data
    while (std::getline(std::cin, line)) {
        std::smatch matches;
        if (std::regex_search(line, matches, valve_regex)) {
            std::string name = matches[1].str();
            int flow_rate = std::stoi(matches[2].str());
            std::string tunnels_str = matches[3].str();

            int current_valve_id;
            // Assign ID if valve name is new
            if (valve_name_to_id.find(name) == valve_name_to_id.end()) {
                current_valve_id = next_id++;
                valve_name_to_id[name] = current_valve_id;
                all_valves.resize(next_id); // Expand vector size as needed
            } else {
                current_valve_id = valve_name_to_id[name];
            }
            all_valves[current_valve_id].name = name;
            all_valves[current_valve_id].flow_rate = flow_rate;

            // Parse and add tunnel connections
            std::stringstream ss(tunnels_str);
            std::string tunnel_valve_name;
            while (std::getline(ss, tunnel_valve_name, ',')) {
                // Trim leading space from valve name
                tunnel_valve_name.erase(0, tunnel_valve_name.find_first_not_of(" ")); 
                int tunnel_valve_id;
                if (valve_name_to_id.find(tunnel_valve_name) == valve_name_to_id.end()) {
                    tunnel_valve_id = next_id++;
                    valve_name_to_id[tunnel_valve_name] = tunnel_valve_id;
                    all_valves.resize(next_id);
                } else {
                    tunnel_valve_id = valve_name_to_id[tunnel_valve_name];
                }
                all_valves[current_valve_id].tunnels_to.push_back(tunnel_valve_id);
            }
        }
    }

    // Phase 2: Identify "important" valves and prepare related data structures
    int start_valve_orig_id = valve_name_to_id["AA"];
    K_important_valves = 0;
    
    // Add all valves with flow_rate > 0 first
    for (int i = 0; i < all_valves.size(); ++i) {
        if (all_valves[i].flow_rate > 0) {
            important_valve_orig_ids.push_back(i);
            original_id_to_important_idx[i] = K_important_valves++;
            important_flow_rates.push_back(all_valves[i].flow_rate);
        }
    }
    // Add AA if it has flow_rate == 0 and is not already counted among useful valves
    bool aa_is_already_important = false;
    for(int id : important_valve_orig_ids) {
        if (id == start_valve_orig_id) {
            aa_is_already_important = true;
            break;
        }
    }
    if (!aa_is_already_important) {
        important_valve_orig_ids.push_back(start_valve_orig_id);
        original_id_to_important_idx[start_valve_orig_id] = K_important_valves++;
        important_flow_rates.push_back(all_valves[start_valve_orig_id].flow_rate); // Its flow rate will be 0
    }
    
    int start_important_idx = original_id_to_important_idx[start_valve_orig_id];

    // Phase 3: Compute all-pairs shortest paths between important valves using BFS
    dist_matrix.resize(K_important_valves, std::vector<int>(K_important_valves, std::numeric_limits<int>::max()));

    for (int i = 0; i < K_important_valves; ++i) {
        int start_node_orig_id = important_valve_orig_ids[i];
        
        std::queue<std::pair<int, int>> q; // {valve_id, distance}
        q.push({start_node_orig_id, 0});
        std::map<int, int> distances; // Tracks distances from start_node_orig_id to all visited nodes
        distances[start_node_orig_id] = 0;

        while (!q.empty()) {
            std::pair<int, int> current = q.front();
            q.pop();
            int u_orig_id = current.first;
            int dist = current.second;

            // If the current node `u_orig_id` is an important valve, store its distance from the BFS source
            auto it_u = original_id_to_important_idx.find(u_orig_id);
            if (it_u != original_id_to_important_idx.end()) {
                dist_matrix[i][it_u->second] = dist;
            }

            // Explore neighbors
            for (int v_orig_id : all_valves[u_orig_id].tunnels_to) {
                if (distances.find(v_orig_id) == distances.end()) { // If not visited
                    distances[v_orig_id] = dist + 1;
                    q.push({v_orig_id, dist + 1});
                }
            }
        }
    }
    
    // Phase 4: Solve Part 1
    // Initialize memo table for Part 1 (max time 30)
    for (int i = 0; i < K_important_valves; ++i) {
        for (int j = 0; j < 31; ++j) { // 0 to 30 minutes
            for (int k = 0; k < (1 << K_important_valves); ++k) {
                memo[i][j][k] = -1;
            }
        }
    }
    int part1_result = dfs_part1(start_important_idx, 30, 0);
    std::cout << part1_result << std::endl;

    // Phase 5: Solve Part 2
    // Initialize memo table for Part 2 (max time 26)
    for (int i = 0; i < K_important_valves; ++i) {
        for (int j = 0; j < 27; ++j) { // 0 to 26 minutes
            for (int k = 0; k < (1 << K_important_valves); ++k) {
                memo_part2[i][j][k] = -1;
            }
        }
    }
    max_pressure_for_mask_achieved.clear(); // Clear the map for Part 2
    dfs_part2(start_important_idx, 26, 0, 0); // Start DFS for Part 2

    int max_combined_pressure = 0;
    // Iterate through all masks and their achieved pressures for agent 1 (you)
    for (auto const& [mask1, pressure1] : max_pressure_for_mask_achieved) {
        // Iterate through all masks and their achieved pressures for agent 2 (elephant)
        for (auto const& [mask2, pressure2] : max_pressure_for_mask_achieved) {
            // If the two masks are disjoint (no shared opened valves)
            if ((mask1 & mask2) == 0) {
                max_combined_pressure = std::max(max_combined_pressure, pressure1 + pressure2);
            }
        }
    }
    std::cout << max_combined_pressure << std::endl;
}

int main() {
    // Redirect standard input from "input.txt"
    freopen("input.txt", "r", stdin);

    // Optimize C++ standard streams for faster input/output
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    solve(); // Call the main solver function

    return 0;
}

