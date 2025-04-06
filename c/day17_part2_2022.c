
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define WIDTH 7
#define MAX_JET_LEN 10100 // Based on typical input sizes, can adjust
#define PROFILE_DEPTH 30 // Depth for cycle detection state
#define INITIAL_MAP_SIZE (1 << 16) // 65536 initial size for hash maps

// --- Data Structures ---

typedef struct {
    int x;
    int y;
} Point;

// Rock shapes defined relative to bottom-left corner
Point rock_shapes[5][5] = {
    {{0,0}, {1,0}, {2,0}, {3,0}},        // Horizontal Line (-) - 4 points
    {{1,0}, {0,1}, {1,1}, {2,1}, {1,2}}, // Plus (+) - 5 points
    {{0,0}, {1,0}, {2,0}, {2,1}, {2,2}}, // Inverted L (⌝) - 5 points
    {{0,0}, {0,1}, {0,2}, {0,3}},        // Vertical Line (|) - 4 points
    {{0,0}, {1,0}, {0,1}, {1,1}}         // Square (■) - 4 points
};
int rock_points_count[5] = {4, 5, 5, 4, 4};

// Represents the chamber using bitmasks for rows (y -> row_mask)
// Using a simple hash map with linear probing
typedef struct {
    long long y;         // Key: y-coordinate
    unsigned char mask; // Value: bitmask of occupied cells (bit 0 = x=0)
    bool occupied;
} ChamberEntry;

ChamberEntry* chamber_map;
long long chamber_map_size;
long long chamber_map_count;

// Represents the state for cycle detection
typedef struct {
    int rock_idx;
    int jet_idx;
    int profile[WIDTH]; // Relative height differences from highest_y
} StateKey;

typedef struct {
    long long rock_num;
    long long height;
} StateValue;

typedef struct {
    StateKey key;
    StateValue value;
    bool occupied;
} StateEntry;

StateEntry* state_map;
long long state_map_size;
long long state_map_count;


// --- Hash Map Functions (Simplified Linear Probing) ---

unsigned long long hash_long(long long key, long long map_size) {
    // Simple modulo hashing, ensure positive result
    unsigned long long h = key >= 0 ? key : -key;
    return h % map_size;
}

unsigned long long hash_state(const StateKey* key, long long map_size) {
    unsigned long long hash = 5381; // djb2 hash
    hash = ((hash << 5) + hash) + key->rock_idx;
    hash = ((hash << 5) + hash) + key->jet_idx;
    for (int i = 0; i < WIDTH; ++i) {
        hash = ((hash << 5) + hash) + key->profile[i];
    }
    return hash % map_size;
}

// Find an entry in the chamber map
ChamberEntry* chamber_map_find(long long y) {
    unsigned long long index = hash_long(y, chamber_map_size);
    unsigned long long start_index = index;
    while (chamber_map[index].occupied) {
        if (chamber_map[index].y == y) {
            return &chamber_map[index];
        }
        index = (index + 1) % chamber_map_size;
        if (index == start_index) break; // Full circle, not found
    }
    return NULL;
}

// Insert/Update an entry in the chamber map (resizing not implemented for simplicity)
void chamber_map_insert(long long y, unsigned char mask) {
    ChamberEntry* existing = chamber_map_find(y);
    if (existing) {
        existing->mask = mask; // Update existing row
        return;
    }

    // Find next empty slot (assumes map won't get completely full)
    unsigned long long index = hash_long(y, chamber_map_size);
     while (chamber_map[index].occupied) {
        index = (index + 1) % chamber_map_size;
    }
    chamber_map[index].y = y;
    chamber_map[index].mask = mask;
    chamber_map[index].occupied = true;
    chamber_map_count++;
    // Add resizing logic here if needed
}

// Get mask or 0 if row doesn't exist
unsigned char chamber_map_get(long long y) {
    ChamberEntry* entry = chamber_map_find(y);
    return entry ? entry->mask : 0;
}

// Find an entry in the state map
StateEntry* state_map_find(const StateKey* key) {
    unsigned long long index = hash_state(key, state_map_size);
    unsigned long long start_index = index;

    while (state_map[index].occupied) {
        if (state_map[index].key.rock_idx == key->rock_idx &&
            state_map[index].key.jet_idx == key->jet_idx &&
            memcmp(state_map[index].key.profile, key->profile, sizeof(key->profile)) == 0)
        {
            return &state_map[index];
        }
        index = (index + 1) % state_map_size;
         if (index == start_index) break; // Full circle
    }
    return NULL;
}

// Insert an entry into the state map
void state_map_insert(const StateKey* key, const StateValue* value) {
     // Assumes key is not already present & map won't get full
    unsigned long long index = hash_state(key, state_map_size);
    while (state_map[index].occupied) {
        index = (index + 1) % state_map_size;
    }
    state_map[index].key = *key;
    state_map[index].value = *value;
    state_map[index].occupied = true;
    state_map_count++;
     // Add resizing logic here if needed
}

// --- Simulation Logic ---

// Checks if the rock (defined by its points) can move in the given direction
// Returns true if move is possible, false otherwise. Updates rock points if true.
bool try_move(Point rock[], int count, int dx, int dy, long long* current_max_y) {
    Point next_pos[5];
    for (int i = 0; i < count; ++i) {
        int next_x = rock[i].x + dx;
        long long next_y = rock[i].y + dy;

        // Check boundaries
        if (next_x < 0 || next_x >= WIDTH || next_y <= 0) {
            return false;
        }

        // Check collision with settled rocks
        unsigned char row_mask = chamber_map_get(next_y);
        if (row_mask & (1 << next_x)) {
            return false;
        }
        next_pos[i].x = next_x;
        next_pos[i].y = next_y;
    }

    // If no collisions, update rock position
    for (int i = 0; i < count; ++i) {
        rock[i] = next_pos[i];
    }
    return true;
}

void get_chamber_profile(long long highest_y, int profile[WIDTH]) {
    for (int x = 0; x < WIDTH; ++x) {
        profile[x] = PROFILE_DEPTH; // Sentinel: not found within depth
        for (int dy = 0; dy < PROFILE_DEPTH; ++dy) {
            long long y = highest_y - dy;
            if (y <= 0) break; // Don't check below floor
            if (chamber_map_get(y) & (1 << x)) {
                profile[x] = dy; // Store depth difference
                break;
            }
        }
    }
}


// --- Main Simulation ---

long long simulate(const char* jet_pattern, int jet_len, long long total_rocks) {
    // Initialize Chamber Map
    chamber_map_size = INITIAL_MAP_SIZE;
    chamber_map = calloc(chamber_map_size, sizeof(ChamberEntry));
    chamber_map_count = 0;
    // Floor at y=0 is implicit, no need to store

    // Initialize State Map
    state_map_size = INITIAL_MAP_SIZE;
    state_map = calloc(state_map_size, sizeof(StateEntry));
    state_map_count = 0;


    long long highest_y = 0;
    int jet_index = 0;
    long long rock_number = 0;
    long long additional_height = 0;
    bool cycle_found = false;

    while (rock_number < total_rocks) {
        int current_rock_idx = rock_number % 5;
        int points_count = rock_points_count[current_rock_idx];
        Point current_rock[5]; // Max 5 points

        // Initial position
        long long start_y = highest_y + 4;
        for (int i = 0; i < points_count; ++i) {
            current_rock[i].x = rock_shapes[current_rock_idx][i].x + 2; // 2 units from left wall
            current_rock[i].y = rock_shapes[current_rock_idx][i].y + start_y;
        }

        // Falling loop
        while (true) {
            // 1. Jet push
            char jet_dir = jet_pattern[jet_index % jet_len];
            jet_index++;
            if (jet_dir == '>') {
                try_move(current_rock, points_count, 1, 0, &highest_y);
            } else if (jet_dir == '<') {
                try_move(current_rock, points_count, -1, 0, &highest_y);
            }

            // 2. Move down
            if (!try_move(current_rock, points_count, 0, -1, &highest_y)) {
                // Rock comes to rest
                for (int i = 0; i < points_count; ++i) {
                    long long y = current_rock[i].y;
                    unsigned char current_mask = chamber_map_get(y);
                    chamber_map_insert(y, current_mask | (1 << current_rock[i].x));
                    if (y > highest_y) {
                        highest_y = y;
                    }
                }
                break; // Next rock
            }
        }

        // Cycle Detection (only after enough rocks and if not already applied)
       if (!cycle_found && rock_number > jet_len ) { // Heuristic: wait a bit for pattern to establish
            StateKey current_state_key;
            current_state_key.rock_idx = current_rock_idx;
            current_state_key.jet_idx = (jet_index -1) % jet_len; // Use jet index *before* incrementing for next rock
            get_chamber_profile(highest_y, current_state_key.profile);

            StateEntry* seen = state_map_find(&current_state_key);

            if (seen) {
                long long prev_rock_num = seen->value.rock_num;
                long long prev_height = seen->value.height;

                long long cycle_len_rocks = rock_number - prev_rock_num;
                long long cycle_height_gain = highest_y - prev_height;

                long long remaining_rocks = total_rocks - rock_number -1; // -1 because current rock just finished
                long long num_cycles = remaining_rocks / cycle_len_rocks;

                if (num_cycles > 0) {
                    additional_height = num_cycles * cycle_height_gain;
                    rock_number += num_cycles * cycle_len_rocks;
                    cycle_found = true; // Apply cycle calculation only once
                }
                 // If num_cycles is 0, just continue simulation normally
                 // Add current state even if cycle is found to potentially find shorter cycles later?
                 // Let's stick to the Python logic: store state if not found or no cycle applied yet.
                 // else { // Add state if not found
                 //    StateValue current_state_value = {rock_number, highest_y};
                 //    state_map_insert(&current_state_key, &current_state_value);
                 // }

            } else {
                 StateValue current_state_value = {rock_number, highest_y};
                 state_map_insert(&current_state_key, &current_state_value);
            }
       }


        rock_number++;
    }

    free(chamber_map);
    free(state_map);

    return highest_y + additional_height;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    char jet_pattern[MAX_JET_LEN];
    if (!fgets(jet_pattern, MAX_JET_LEN, fp)) {
         fprintf(stderr, "Error reading jet pattern or file empty.\n");
         fclose(fp);
         return 1;
    }
    fclose(fp);

    // Remove trailing newline if present
    jet_pattern[strcspn(jet_pattern, "\r\n")] = 0;
    int jet_len = strlen(jet_pattern);

    if (jet_len == 0) {
        fprintf(stderr, "Jet pattern is empty.\n");
        return 1;
    }

    long long total_rocks = 1000000000000LL;
    long long final_height = simulate(jet_pattern, jet_len, total_rocks);

    printf("%lld\n", final_height);

    return 0;
}
