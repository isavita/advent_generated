
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_FLOORS 4
#define MAX_MATERIALS 7 // 5 initial + 2 for part 2
#define MAX_ITEMS (MAX_MATERIALS * 2)
#define MAX_LINE_LEN 256
#define INITIAL_QUEUE_CAPACITY 100000
#define INITIAL_VISITED_CAPACITY 1000000

typedef struct {
    int material_id;
    int is_chip; // 0 for generator, 1 for chip
} Item;

typedef struct {
    Item floors[MAX_FLOORS][MAX_ITEMS];
    int floor_counts[MAX_FLOORS];
    int elevator_level;
    int steps;
    int num_materials; // Track total number of unique materials
} State;

typedef struct {
    int gen_floor;
    int chip_floor;
} FloorPair;

// --- Global Material Tracking ---
char material_names[MAX_MATERIALS][30];
int material_count = 0;

int find_or_add_material(const char* name) {
    for (int i = 0; i < material_count; ++i) {
        if (strcmp(material_names[i], name) == 0) {
            return i;
        }
    }
    if (material_count < MAX_MATERIALS) {
        strncpy(material_names[material_count], name, 29);
        material_names[material_count][29] = '\0';
        return material_count++;
    }
    fprintf(stderr, "Error: Too many materials\n");
    exit(EXIT_FAILURE);
}

// --- Queue Implementation ---
State* queue = NULL;
size_t queue_start = 0;
size_t queue_end = 0;
size_t queue_capacity = 0;

void init_queue() {
    queue_capacity = INITIAL_QUEUE_CAPACITY;
    queue = malloc(queue_capacity * sizeof(State));
    if (!queue) {
        perror("Failed to allocate queue");
        exit(EXIT_FAILURE);
    }
    queue_start = 0;
    queue_end = 0;
}

void enqueue(State s) {
    if (queue_end >= queue_capacity) {
        queue_capacity *= 2;
        State* new_queue = realloc(queue, queue_capacity * sizeof(State));
        if (!new_queue) {
            perror("Failed to reallocate queue");
            free(queue);
            exit(EXIT_FAILURE);
        }
        queue = new_queue;
    }
    queue[queue_end++] = s; // Copies the state struct
}

State dequeue() {
    if (queue_start < queue_end) {
        return queue[queue_start++];
    }
    fprintf(stderr, "Error: Dequeue from empty queue\n");
    exit(EXIT_FAILURE);
}

int queue_empty() {
    return queue_start == queue_end;
}

void free_queue() {
    free(queue);
    queue = NULL;
    queue_capacity = 0;
    queue_start = 0;
    queue_end = 0;
}

// --- Visited Set (using Hashing and Sorted Array) ---
uint64_t* visited_hashes = NULL;
size_t visited_count = 0;
size_t visited_capacity = 0;

int compare_u64(const void* a, const void* b) {
    uint64_t ua = *(const uint64_t*)a;
    uint64_t ub = *(const uint64_t*)b;
    if (ua < ub) return -1;
    if (ua > ub) return 1;
    return 0;
}

void init_visited() {
    visited_capacity = INITIAL_VISITED_CAPACITY;
    visited_hashes = malloc(visited_capacity * sizeof(uint64_t));
    if (!visited_hashes) {
        perror("Failed to allocate visited set");
        exit(EXIT_FAILURE);
    }
    visited_count = 0;
}

int add_visited(uint64_t hash) {
    // Use bsearch to check if hash already exists
    if (bsearch(&hash, visited_hashes, visited_count, sizeof(uint64_t), compare_u64)) {
        return 0; // Already visited
    }

    // Resize if necessary
    if (visited_count >= visited_capacity) {
        visited_capacity *= 2;
        uint64_t* new_visited = realloc(visited_hashes, visited_capacity * sizeof(uint64_t));
        if (!new_visited) {
            perror("Failed to reallocate visited set");
            free(visited_hashes);
            exit(EXIT_FAILURE);
        }
        visited_hashes = new_visited;
    }

    // Find insertion point to maintain sorted order
    size_t low = 0, high = visited_count;
    while(low < high) {
        size_t mid = low + (high - low) / 2;
        if (visited_hashes[mid] < hash) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    size_t insert_pos = low;


    // Shift elements to make space
    if (insert_pos < visited_count) {
         memmove(&visited_hashes[insert_pos + 1], &visited_hashes[insert_pos], (visited_count - insert_pos) * sizeof(uint64_t));
    }

    // Insert the new hash
    visited_hashes[insert_pos] = hash;
    visited_count++;
    return 1; // Added successfully
}


void free_visited() {
    free(visited_hashes);
    visited_hashes = NULL;
    visited_capacity = 0;
    visited_count = 0;
}

// --- State Helper Functions ---

int compare_floor_pairs(const void* a, const void* b) {
    const FloorPair* pa = (const FloorPair*)a;
    const FloorPair* pb = (const FloorPair*)b;
    if (pa->gen_floor != pb->gen_floor) return pa->gen_floor - pb->gen_floor;
    return pa->chip_floor - pb->chip_floor;
}

uint64_t hash_key(const State* s) {
     uint64_t hash = s->elevator_level;
     FloorPair pairs[MAX_MATERIALS];
     int gen_floors[MAX_MATERIALS];
     int chip_floors[MAX_MATERIALS];
     memset(gen_floors, -1, sizeof(gen_floors));
     memset(chip_floors, -1, sizeof(chip_floors));

     for (int f = 0; f < MAX_FLOORS; ++f) {
         for (int i = 0; i < s->floor_counts[f]; ++i) {
              Item item = s->floors[f][i];
             if (item.is_chip) chip_floors[item.material_id] = f;
             else gen_floors[item.material_id] = f;
         }
     }
     for (int i = 0; i < s->num_materials; ++i) {
         pairs[i].gen_floor = gen_floors[i];
         pairs[i].chip_floor = chip_floors[i];
     }
     qsort(pairs, s->num_materials, sizeof(FloorPair), compare_floor_pairs);

     // Combine sorted pairs into hash (use a simple prime multiplier)
     for (int i = 0; i < s->num_materials; i++) {
          hash = hash * 31 + pairs[i].gen_floor;
          hash = hash * 31 + pairs[i].chip_floor;
     }
     return hash;
}


int is_valid(const State* s) {
    for (int f = 0; f < MAX_FLOORS; ++f) {
        int has_unpaired_chip = 0;
        int has_generator = 0;
        int gen_present[MAX_MATERIALS] = {0};

        for (int i = 0; i < s->floor_counts[f]; ++i) {
            Item item = s->floors[f][i];
            if (!item.is_chip) {
                has_generator = 1;
                gen_present[item.material_id] = 1;
            }
        }

        if (!has_generator) continue; // No generators, chips are safe

        for (int i = 0; i < s->floor_counts[f]; ++i) {
             Item item = s->floors[f][i];
             if (item.is_chip && !gen_present[item.material_id]) {
                 has_unpaired_chip = 1;
                 break;
             }
        }

        if (has_unpaired_chip) return 0; // Unpaired chip with other generator present
    }
    return 1;
}

int is_done(const State* s) {
    for (int f = 0; f < MAX_FLOORS - 1; ++f) {
        if (s->floor_counts[f] > 0) {
            return 0;
        }
    }
    return 1;
}

State clone_state(const State* s) {
    State cl = *s; // Shallow copy is enough as floors are fixed arrays in struct
    // If floors were pointers, deep copy would be needed here.
    return cl;
}

// --- Main Logic ---

int solve() {
    init_queue();
    init_visited();

    // Read input and create initial state
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return -1;
    }

    State initial_state;
    memset(&initial_state, 0, sizeof(State));
    initial_state.elevator_level = 0;
    initial_state.steps = 0;

    char line[MAX_LINE_LEN];
    int current_floor = 0;
    while (fgets(line, sizeof(line), file) && current_floor < MAX_FLOORS) {
        char* token;
        char* rest = line;
        char material_buf[30];

        while ((token = strtok_r(rest, " \t\n.,;-", &rest))) {
             if (strcmp(token, "generator") == 0) {
                 // The previous significant token should be the material
                 Item item;
                 item.is_chip = 0;
                 item.material_id = find_or_add_material(material_buf);
                 initial_state.floors[current_floor][initial_state.floor_counts[current_floor]++] = item;
             } else if (strcmp(token, "microchip") == 0) {
                 // The previous significant token was material-compatible
                 Item item;
                 item.is_chip = 1;
                 item.material_id = find_or_add_material(material_buf);
                 initial_state.floors[current_floor][initial_state.floor_counts[current_floor]++] = item;
             } else if (strcmp(token, "relevant") != 0 && strcmp(token, "a") != 0 &&
                        strcmp(token, "and") != 0 && strcmp(token, "floor") != 0 &&
                        strcmp(token, "contains") != 0 && strcmp(token, "The") != 0 &&
                        strcmp(token, "nothing") != 0 && strcmp(token, "first") != 0 &&
                        strcmp(token, "second") != 0 && strcmp(token, "third") != 0 &&
                        strcmp(token, "fourth") != 0 && strcmp(token, "compatible") != 0)
             {
                 // Assume it's a material name (potentially hyphenated like 'strontium-compatible')
                 char* hyphen = strchr(token, '-');
                 if (hyphen) *hyphen = '\0'; // Cut off "-compatible"
                 strncpy(material_buf, token, 29);
                 material_buf[29] = '\0';
             }
        }
        current_floor++;
    }
    fclose(file);
    initial_state.num_materials = material_count;


    // --- BFS Start ---
    enqueue(initial_state);
    add_visited(hash_key(&initial_state));

    while (!queue_empty()) {
        State current = dequeue();

        if (is_done(&current)) {
            free_queue();
            free_visited();
            return current.steps;
        }

        int current_level = current.elevator_level;
        int current_floor_count = current.floor_counts[current_level];

        int ele_diffs[2];
        int num_diffs = 0;
        if (current_level < MAX_FLOORS - 1) ele_diffs[num_diffs++] = 1;
        if (current_level > 0) ele_diffs[num_diffs++] = -1;

        // Iterate through possible moves (1 or 2 items)
        int items_to_move_indices[2];
        for (int i = 0; i < current_floor_count; ++i) {
            // Move 1 item
            items_to_move_indices[0] = i;
            int num_items_to_move = 1;

            for(int d = 0; d < num_diffs; ++d) {
                 int ele_diff = ele_diffs[d];
                 int next_level = current_level + ele_diff;

                 State next_state = clone_state(&current);
                 next_state.elevator_level = next_level;
                 next_state.steps++;

                 // Move items logically
                 Item items_moved[2];
                 int moved_count = 0;
                 int current_floor_original_count = next_state.floor_counts[current_level];

                 // Add to new floor first
                 for(int k=0; k < num_items_to_move; ++k) {
                     int item_idx = items_to_move_indices[k];
                     items_moved[moved_count++] = next_state.floors[current_level][item_idx];
                     next_state.floors[next_level][next_state.floor_counts[next_level]++] = items_moved[moved_count-1];
                 }

                 // Remove from old floor (tricky part: indices shift)
                 // Sort indices descending for easier removal
                  if (num_items_to_move == 2 && items_to_move_indices[0] < items_to_move_indices[1]) {
                     int tmp = items_to_move_indices[0];
                     items_to_move_indices[0] = items_to_move_indices[1];
                     items_to_move_indices[1] = tmp;
                 }

                 for (int k = 0; k < num_items_to_move; ++k) {
                    int remove_idx = items_to_move_indices[k];
                    // Overwrite removed item with the last item, then decrease count
                    next_state.floors[current_level][remove_idx] = next_state.floors[current_level][next_state.floor_counts[current_level]-1];
                    next_state.floor_counts[current_level]--;
                 }


                 if (is_valid(&next_state)) {
                     uint64_t next_hash = hash_key(&next_state);
                     if (add_visited(next_hash)) {
                         enqueue(next_state);
                     }
                 }
             }

             // Move 2 items
             for (int j = i + 1; j < current_floor_count; ++j) {
                items_to_move_indices[0] = i;
                items_to_move_indices[1] = j;
                num_items_to_move = 2;

                 for(int d = 0; d < num_diffs; ++d) {
                     int ele_diff = ele_diffs[d];
                     int next_level = current_level + ele_diff;

                     State next_state = clone_state(&current);
                     next_state.elevator_level = next_level;
                     next_state.steps++;

                     Item items_moved[2];
                     int moved_count = 0;
                     int current_floor_original_count = next_state.floor_counts[current_level];

                      // Add to new floor first
                     for(int k=0; k < num_items_to_move; ++k) {
                         int item_idx = items_to_move_indices[k];
                         items_moved[moved_count++] = next_state.floors[current_level][item_idx];
                          next_state.floors[next_level][next_state.floor_counts[next_level]++] = items_moved[moved_count-1];
                     }

                     // Remove from old floor (indices shift)
                     // Sort indices descending for easier removal
                      if (num_items_to_move == 2 && items_to_move_indices[0] < items_to_move_indices[1]) {
                         int tmp = items_to_move_indices[0];
                         items_to_move_indices[0] = items_to_move_indices[1];
                         items_to_move_indices[1] = tmp;
                     }

                     for (int k = 0; k < num_items_to_move; ++k) {
                        int remove_idx = items_to_move_indices[k];
                        next_state.floors[current_level][remove_idx] = next_state.floors[current_level][next_state.floor_counts[current_level]-1];
                        next_state.floor_counts[current_level]--;
                     }

                     if (is_valid(&next_state)) {
                         uint64_t next_hash = hash_key(&next_state);
                         if (add_visited(next_hash)) {
                             enqueue(next_state);
                         }
                     }
                 }
             }
        }
    }

    // Should not be reached if a solution exists
    free_queue();
    free_visited();
    return -1;
}

int main() {
    int result = solve();
    if (result != -1) {
        printf("%d\n", result);
    } else {
        printf("No solution found.\n");
    }
    return 0;
}

