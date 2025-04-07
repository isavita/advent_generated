
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_VALVES 60
#define MAX_LINE_LEN 256
#define MAX_TIME 30
#define MAX_USEFUL_VALVES 16 // Max 15-16 for 1 << 16 memoization
#define INF 1000000

typedef struct {
    char id[3];
    int flow;
    int useful_idx; // Index in the useful_valves array, or -1
} ValveInfo;

ValveInfo valves[MAX_VALVES];
int dist[MAX_VALVES][MAX_VALVES];
int num_valves = 0;

int useful_indices[MAX_USEFUL_VALVES];
int num_useful = 0;

long long memo[MAX_VALVES][MAX_TIME + 1][1 << MAX_USEFUL_VALVES];

int get_valve_index(const char *id) {
    for (int i = 0; i < num_valves; ++i) {
        if (strcmp(valves[i].id, id) == 0) {
            return i;
        }
    }
    strncpy(valves[num_valves].id, id, 2);
    valves[num_valves].id[2] = '\0';
    valves[num_valves].flow = 0;
    valves[num_valves].useful_idx = -1;
    return num_valves++;
}

long long max_ll(long long a, long long b) {
    return (a > b) ? a : b;
}

long long solve(int u_idx, int time_left, unsigned int mask) {
    if (time_left <= 0) {
        return 0;
    }
    if (memo[u_idx][time_left][mask] != -1) {
        return memo[u_idx][time_left][mask];
    }

    long long max_pressure = 0;

    for (int i = 0; i < num_useful; ++i) {
        if (mask & (1U << i)) {
            int v_idx = useful_indices[i];
            int travel_time = dist[u_idx][v_idx];
            int time_needed = travel_time + 1; // Travel + open valve

            if (time_left > time_needed) {
                 int next_time_left = time_left - time_needed;
                 long long current_release = (long long)valves[v_idx].flow * next_time_left;
                 unsigned int next_mask = mask & ~(1U << i);
                 max_pressure = max_ll(max_pressure, current_release + solve(v_idx, next_time_left, next_mask));
            }
        }
    }

    return memo[u_idx][time_left][mask] = max_pressure;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening file");
        return 1;
    }

    for (int i = 0; i < MAX_VALVES; ++i) {
        for (int j = 0; j < MAX_VALVES; ++j) {
            dist[i][j] = (i == j) ? 0 : INF;
        }
    }

    char line[MAX_LINE_LEN];
    char valve_id[3];
    int flow;
    char tunnels_str[MAX_LINE_LEN];

    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\n")] = 0; // Remove trailing newline

        sscanf(line, "Valve %2s has flow rate=%d;", valve_id, &flow);
        int u_idx = get_valve_index(valve_id);
        valves[u_idx].flow = flow;

        char *tunnels_part = strstr(line, "to valve");
        if (tunnels_part) {
             if (tunnels_part[8] == 's') { // handles "valves" vs "valve"
                tunnels_part += 10;
             } else {
                tunnels_part += 9;
             }

            char *token = strtok(tunnels_part, ", ");
            while (token != NULL) {
                int v_idx = get_valve_index(token);
                dist[u_idx][v_idx] = 1;
                token = strtok(NULL, ", ");
            }
        }
    }
    fclose(fp);

    for (int k = 0; k < num_valves; ++k) {
        for (int i = 0; i < num_valves; ++i) {
            for (int j = 0; j < num_valves; ++j) {
                if (dist[i][k] != INF && dist[k][j] != INF) {
                     int new_dist = dist[i][k] + dist[k][j];
                     if (new_dist < dist[i][j]) {
                         dist[i][j] = new_dist;
                     }
                }
            }
        }
    }

    for (int i = 0; i < num_valves; ++i) {
        if (valves[i].flow > 0) {
            if (num_useful < MAX_USEFUL_VALVES) {
                 valves[i].useful_idx = num_useful;
                 useful_indices[num_useful++] = i;
            } else {
                 fprintf(stderr, "Error: Exceeded MAX_USEFUL_VALVES limit.\n");
                 return 1; // Or handle differently
            }
        }
    }

    int start_idx = get_valve_index("AA");
    unsigned int initial_mask = (1U << num_useful) - 1;

    for(int i=0; i<MAX_VALVES; ++i) {
        for(int j=0; j<=MAX_TIME; ++j) {
            for(int k=0; k < (1 << num_useful); ++k) {
                memo[i][j][k] = -1;
            }
        }
    }

    long long result = solve(start_idx, MAX_TIME, initial_mask);
    printf("%lld\n", result);

    return 0;
}

