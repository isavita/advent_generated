
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_VALVES 60
#define MAX_ID_LEN 3
#define TIME_LIMIT 26
#define INF 1000000 // Represents infinity for distances

typedef struct {
    char id[MAX_ID_LEN];
    int flow;
    int index;         // Index in the valves array
    int open_mask_idx; // Index for the bitmask (0..num_open_valves-1), or -1 if flow=0
} Valve;

Valve valves[MAX_VALVES];
int num_valves = 0;
int dist[MAX_VALVES][MAX_VALVES];
int open_valve_indices[MAX_VALVES]; // Stores indices of valves with flow > 0
int num_open_valves = 0;

// Memoization table: memo[valve_idx][time_left][open_mask]
// Max 16 open valves assumed for mask size (adjust if needed)
int memo[MAX_VALVES][TIME_LIMIT + 1][1 << 16];

// Find valve index by ID
int find_valve_index(const char *id) {
    for (int i = 0; i < num_valves; ++i) {
        if (strncmp(valves[i].id, id, MAX_ID_LEN) == 0) {
            return i;
        }
    }
    return -1; // Not found
}

// Max helper function
int max(int a, int b) {
    return (a > b) ? a : b;
}

// Recursive function to calculate max pressure for one agent
int solve(int curr_idx, int time_left, int open_mask) {
    if (time_left <= 0) {
        return 0;
    }
    if (memo[curr_idx][time_left][open_mask] != -1) {
        return memo[curr_idx][time_left][open_mask];
    }

    int max_pressure = 0;

    // Iterate through valves this agent can potentially open
    for (int i = 0; i < num_open_valves; ++i) {
        // Check if the i-th openable valve is in our mask
        if ((open_mask >> i) & 1) {
            int next_valve_idx = open_valve_indices[i];
            int travel_time = dist[curr_idx][next_valve_idx];
            int time_after_action = time_left - travel_time - 1; // -1 for opening

            if (time_after_action > 0) {
                int current_pressure = valves[next_valve_idx].flow * time_after_action;
                int remaining_mask = open_mask & ~(1 << i); // Remove opened valve from mask
                max_pressure = max(max_pressure,
                                   current_pressure + solve(next_valve_idx, time_after_action, remaining_mask));
            }
        }
    }

    memo[curr_idx][time_left][open_mask] = max_pressure;
    return max_pressure;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char line[256];
    char valve_id[MAX_ID_LEN];
    int flow_rate;
    char tunnels_str[200];

    // Initialize distances
    for (int i = 0; i < MAX_VALVES; ++i) {
        for (int j = 0; j < MAX_VALVES; ++j) {
            dist[i][j] = (i == j) ? 0 : INF;
        }
        valves[i].open_mask_idx = -1; // Default: not an openable valve
    }

    // First pass: Read valves and direct tunnels
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline

        // Simplified parsing assuming specific format
        sscanf(line, "Valve %2s has flow rate=%d;", valve_id, &flow_rate);

        int current_idx = find_valve_index(valve_id);
        if (current_idx == -1) {
            current_idx = num_valves++;
            strncpy(valves[current_idx].id, valve_id, MAX_ID_LEN);
            valves[current_idx].index = current_idx;
        }
        valves[current_idx].flow = flow_rate;

        // Find tunnels part
        char *tunnels_ptr = strstr(line, "to valve");
        if (tunnels_ptr) {
            // Move past "to valve" or "to valves"
            tunnels_ptr += (strstr(line, "valves") ? 9 : 8);
            strncpy(tunnels_str, tunnels_ptr, sizeof(tunnels_str) - 1);
            tunnels_str[sizeof(tunnels_str) - 1] = '\0'; // Ensure null termination

            char *token = strtok(tunnels_str, ", ");
            while (token) {
                if (strlen(token) > 0) {
                     int neighbor_idx = find_valve_index(token);
                     if(neighbor_idx == -1){
                         neighbor_idx = num_valves++;
                         strncpy(valves[neighbor_idx].id, token, MAX_ID_LEN);
                         valves[neighbor_idx].index = neighbor_idx;
                     }
                     dist[current_idx][neighbor_idx] = 1; // Direct tunnel
                }
                token = strtok(NULL, ", ");
            }
        }
    }
    fclose(file);

    // Floyd-Warshall for all-pairs shortest paths
    for (int k = 0; k < num_valves; ++k) {
        for (int i = 0; i < num_valves; ++i) {
            for (int j = 0; j < num_valves; ++j) {
                if (dist[i][k] != INF && dist[k][j] != INF) {
                    if (dist[i][j] > dist[i][k] + dist[k][j]) {
                        dist[i][j] = dist[i][k] + dist[k][j];
                    }
                }
            }
        }
    }

    // Identify valves with positive flow rate and assign mask indices
    num_open_valves = 0;
    for (int i = 0; i < num_valves; ++i) {
        if (valves[i].flow > 0) {
            valves[i].open_mask_idx = num_open_valves;
            open_valve_indices[num_open_valves++] = i;
        }
    }

    // Initialize memoization table
    memset(memo, -1, sizeof(memo));

    int start_idx = find_valve_index("AA");
    if (start_idx == -1) {
        fprintf(stderr, "Start valve 'AA' not found.\n");
        return 1;
    }

    int max_total_pressure = 0;
    int total_masks = (1 << num_open_valves); // 2^num_open_valves

    // Iterate through all possible ways to split the openable valves
    for (int my_mask = 0; my_mask < total_masks; ++my_mask) {
        // The elephant gets the valves not taken by me
        int elephant_mask = (total_masks - 1) ^ my_mask;

        int my_pressure = solve(start_idx, TIME_LIMIT, my_mask);
        int elephant_pressure = solve(start_idx, TIME_LIMIT, elephant_mask);

        max_total_pressure = max(max_total_pressure, my_pressure + elephant_pressure);
    }

    printf("%d\n", max_total_pressure);

    return 0;
}
