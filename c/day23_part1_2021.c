
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>
#include <math.h>

#define ROWS 5
#define COLS 13
#define HEAP_CAPACITY 1000000 // Adjust capacity as needed
#define HASH_TABLE_SIZE 2000000 // Adjust size as needed

typedef struct {
    int r, c;
} Coord;

typedef struct {
    char grid[ROWS][COLS];
    int energy_used;
} State;

// --- Min Heap Implementation ---
typedef struct {
    State* data[HEAP_CAPACITY];
    int size;
} MinHeap;

void swap_heap_nodes(State** a, State** b) {
    State* temp = *a;
    *a = *b;
    *b = temp;
}

void heapify_up(MinHeap* heap, int index) {
    while (index > 0) {
        int parent_index = (index - 1) / 2;
        if (heap->data[index]->energy_used < heap->data[parent_index]->energy_used) {
            swap_heap_nodes(&heap->data[index], &heap->data[parent_index]);
            index = parent_index;
        } else {
            break;
        }
    }
}

void heapify_down(MinHeap* heap, int index) {
    int smallest = index;
    int left_child = 2 * index + 1;
    int right_child = 2 * index + 2;

    if (left_child < heap->size && heap->data[left_child]->energy_used < heap->data[smallest]->energy_used) {
        smallest = left_child;
    }
    if (right_child < heap->size && heap->data[right_child]->energy_used < heap->data[smallest]->energy_used) {
        smallest = right_child;
    }

    if (smallest != index) {
        swap_heap_nodes(&heap->data[index], &heap->data[smallest]);
        heapify_down(heap, smallest);
    }
}

void heap_push(MinHeap* heap, State* state) {
    if (heap->size >= HEAP_CAPACITY) {
        fprintf(stderr, "Heap capacity exceeded\n");
        exit(EXIT_FAILURE); // Or resize dynamically
    }
    heap->data[heap->size] = state;
    heap->size++;
    heapify_up(heap, heap->size - 1);
}

State* heap_pop(MinHeap* heap) {
    if (heap->size == 0) {
        return NULL;
    }
    State* min_state = heap->data[0];
    heap->data[0] = heap->data[heap->size - 1];
    heap->size--;
    heapify_down(heap, 0);
    return min_state;
}

// --- Hash Table for Seen States ---
typedef struct HashNode {
    State state;
    struct HashNode* next;
} HashNode;

HashNode* hash_table[HASH_TABLE_SIZE];

unsigned long hash_grid(const char grid[ROWS][COLS]) {
    unsigned long hash = 5381;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            hash = ((hash << 5) + hash) + grid[r][c]; // djb2 hash
        }
    }
    return hash % HASH_TABLE_SIZE;
}

bool grid_equal(const char g1[ROWS][COLS], const char g2[ROWS][COLS]) {
    return memcmp(g1, g2, sizeof(char) * ROWS * COLS) == 0;
}

bool seen_lookup_or_insert(State* state) {
    unsigned long index = hash_grid(state->grid);
    HashNode* current = hash_table[index];
    while (current != NULL) {
        if (grid_equal(current->state.grid, state->grid)) {
            // Found: check if the new path is better (lower energy)
            if (state->energy_used >= current->state.energy_used) {
                 return true; // Already seen with equal or better energy
            } else {
                 // Update the energy of the seen state (Dijkstra relaxation)
                 current->state.energy_used = state->energy_used;
                 return false; // Need to re-add to heap with better energy
            }
        }
        current = current->next;
    }

    // Not found: insert
    HashNode* new_node = (HashNode*)malloc(sizeof(HashNode));
    if (!new_node) {
        perror("Failed to allocate hash node");
        exit(EXIT_FAILURE);
    }
    memcpy(new_node->state.grid, state->grid, sizeof(state->grid));
    new_node->state.energy_used = state->energy_used; // Store energy too
    new_node->next = hash_table[index];
    hash_table[index] = new_node;
    return false; // Indicate it was not seen before (or is now updated)
}

// --- Amphipod Logic ---

const int energy_per_type[] = {'A', 1, 'B', 10, 'C', 100, 'D', 1000};
const int room_cols[] = {3, 5, 7, 9};
const char target_chars[] = {'A', 'B', 'C', 'D'};

int get_energy_cost(char type) {
    for (int i = 0; i < 8; i += 2) {
        if (energy_per_type[i] == type) {
            return energy_per_type[i + 1];
        }
    }
    return 0; // Should not happen
}

char get_target_char_for_col(int col) {
     for(int i=0; i<4; ++i) {
         if (room_cols[i] == col) return target_chars[i];
     }
     return '\0'; // Not a room column
}

bool is_target_room(int r, int c, char amphipod_type) {
    if (r < 2 || r >= ROWS -1) return false; // Must be in a room row
    return get_target_char_for_col(c) == amphipod_type;
}


bool all_done(const char grid[ROWS][COLS]) {
    for (int i = 0; i < 4; ++i) {
        int col = room_cols[i];
        char target = target_chars[i];
        for (int row = 2; row < ROWS - 1; ++row) {
             if (grid[row][col] != target) return false;
        }
    }
    return true;
}

// Simple BFS for path finding within get_next_possible_moves
typedef struct {
    Coord coord;
    int dist;
} BFSNode;

#define MAX_QUEUE_SIZE (ROWS * COLS)

typedef struct {
    BFSNode items[MAX_QUEUE_SIZE];
    int front, rear, size;
} Queue;

void queue_init(Queue* q) {
    q->front = 0;
    q->rear = -1;
    q->size = 0;
}

bool queue_is_empty(Queue* q) {
    return q->size == 0;
}

void queue_push(Queue* q, BFSNode item) {
    if (q->size >= MAX_QUEUE_SIZE) return; // Should not happen
    q->rear = (q->rear + 1) % MAX_QUEUE_SIZE;
    q->items[q->rear] = item;
    q->size++;
}

BFSNode queue_pop(Queue* q) {
    BFSNode item = q->items[q->front];
    q->front = (q->front + 1) % MAX_QUEUE_SIZE;
    q->size--;
    return item;
}


int get_next_possible_moves(const State* current_state, Coord start_coord, Coord* possible_moves, int* move_dists) {
    int count = 0;
    char amphipod_type = current_state->grid[start_coord.r][start_coord.c];
    int energy_mult = get_energy_cost(amphipod_type);
    bool started_in_hallway = (start_coord.r == 1);

    bool visited[ROWS][COLS] = {false};
    Queue q;
    queue_init(&q);

    queue_push(&q, (BFSNode){start_coord, 0});
    visited[start_coord.r][start_coord.c] = true;

    int dr[] = {-1, 1, 0, 0};
    int dc[] = {0, 0, -1, 1};

    while (!queue_is_empty(&q)) {
        BFSNode current = queue_pop(&q);
        Coord current_c = current.coord;
        int current_dist = current.dist;

        // Check if this is a valid destination
        if (current_dist > 0) { // Cannot move to the start position
            if (started_in_hallway) {
                // Must move into the correct room, and only if the room is ready
                if (is_target_room(current_c.r, current_c.c, amphipod_type)) {
                    bool room_ready = true;
                    // Check deeper spots in the room
                    for (int r = current_c.r + 1; r < ROWS - 1; ++r) {
                        if (current_state->grid[r][current_c.c] == '.') {
                             room_ready = false; // Cannot stop above empty space
                             break;
                        }
                        if (current_state->grid[r][current_c.c] != amphipod_type) {
                             room_ready = false; // Cannot enter room with wrong types
                             break;
                        }
                    }
                     // Must move to the deepest available spot
                    bool deeper_available = false;
                    for (int r = current_c.r + 1; r < ROWS -1; ++r) {
                         if (current_state->grid[r][current_c.c] == '.') {
                             deeper_available = true;
                             break;
                         }
                    }


                    if (room_ready && !deeper_available) {
                       possible_moves[count] = current_c;
                       move_dists[count] = current_dist * energy_mult;
                       count++;
                    }
                }
            } else { // Started in a room
                 // Can move to any valid hallway spot (not above rooms)
                 if (current_c.r == 1) { // Hallway
                     bool is_entrance = false;
                     for(int i=0; i<4; ++i) if (current_c.c == room_cols[i]) is_entrance = true;
                     if (!is_entrance) {
                         possible_moves[count] = current_c;
                         move_dists[count] = current_dist * energy_mult;
                         count++;
                     }
                 }
                 // Cannot move from one room to another directly.
            }
        }

        // Explore neighbors
        for (int i = 0; i < 4; ++i) {
            int next_r = current_c.r + dr[i];
            int next_c = current_c.c + dc[i];

            if (next_r >= 0 && next_r < ROWS && next_c >= 0 && next_c < COLS &&
                current_state->grid[next_r][next_c] == '.' && !visited[next_r][next_c])
            {
                visited[next_r][next_c] = true;
                queue_push(&q, (BFSNode){{next_r, next_c}, current_dist + 1});
            }
        }
    }
    return count;
}

bool is_amphipod(char c) {
    return c >= 'A' && c <= 'D';
}

void solve() {
    State initial_state = { .energy_used = 0 };
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        exit(EXIT_FAILURE);
    }
    for (int r = 0; r < ROWS; ++r) {
        if (!fgets(initial_state.grid[r], COLS + 2, fp)) { // +2 for potential \n and \0
             // Handle potential short read or error if needed
             break;
        }
        // Remove trailing newline if present
        initial_state.grid[r][strcspn(initial_state.grid[r], "\n")] = 0;
         // Pad shorter lines with spaces if necessary (though input format is usually fixed)
        int len = strlen(initial_state.grid[r]);
        for(int c = len; c < COLS; ++c) {
             initial_state.grid[r][c] = ' '; // Use space for padding outside bounds
        }
         initial_state.grid[r][COLS] = '\0'; // Ensure null termination
    }
    fclose(fp);


    // --- Dijkstra ---
    MinHeap heap = { .size = 0 };
    memset(hash_table, 0, sizeof(hash_table)); // Initialize hash table

    State* start_node = (State*)malloc(sizeof(State));
    if (!start_node) {perror("malloc"); exit(1);}
    memcpy(start_node, &initial_state, sizeof(State));

    heap_push(&heap, start_node);
    seen_lookup_or_insert(start_node); // Mark start as seen

    int min_energy = -1;

    while (heap.size > 0) {
        State* current_p = heap_pop(&heap);
        State current = *current_p; // Work with a copy on the stack
        // No need to free current_p yet, it might be pointed to by hash table

        if (all_done(current.grid)) {
            min_energy = current.energy_used;
            // Free the winning state node before breaking
            // Need careful memory management if we break early.
            // Let's clean up everything at the end for simplicity.
            break;
        }

       // Find all amphipods that might need to move
        for (int r = 1; r < ROWS - 1; ++r) {
            for (int c = 1; c < COLS - 1; ++c) {
                char amphipod_type = current.grid[r][c];
                if (!is_amphipod(amphipod_type)) continue;

                Coord start_coord = {r, c};
                bool in_hallway = (r == 1);
                bool in_correct_room = is_target_room(r, c, amphipod_type);

                // Check if needs moving:
                // 1. In hallway? Always needs to move eventually.
                // 2. In wrong room? Needs to move.
                // 3. In correct room but blocking others? Needs to move.
                bool needs_moving = false;
                if (in_hallway) {
                    needs_moving = true;
                } else if (!in_correct_room) {
                     needs_moving = true;
                } else { // In correct room, check if blocking
                    for (int below_r = r + 1; below_r < ROWS - 1; ++below_r) {
                         if (current.grid[below_r][c] != amphipod_type) {
                              needs_moving = true; // Blocking a wrong type or empty space below
                              break;
                         }
                    }
                }

                if (!needs_moving) continue;


                Coord possible_moves[ROWS*COLS];
                int move_dists[ROWS*COLS]; // Stores energy cost directly
                int num_moves = get_next_possible_moves(&current, start_coord, possible_moves, move_dists);


                for (int i = 0; i < num_moves; ++i) {
                    Coord next_coord = possible_moves[i];
                    int move_energy = move_dists[i];

                    // Create next state
                    State* next_state_p = (State*)malloc(sizeof(State));
                     if (!next_state_p) {perror("malloc"); exit(1);}
                    memcpy(next_state_p->grid, current.grid, sizeof(current.grid));
                    next_state_p->energy_used = current.energy_used + move_energy;

                    // Perform move
                    next_state_p->grid[next_coord.r][next_coord.c] = amphipod_type;
                    next_state_p->grid[start_coord.r][start_coord.c] = '.';


                    // Check if seen or insert/update
                    if (!seen_lookup_or_insert(next_state_p)) {
                          // If it was inserted or updated with lower energy, push to heap
                          heap_push(&heap, next_state_p);
                    } else {
                         // Already seen with better or equal energy, discard this path
                         free(next_state_p);
                    }
                }
            }
        }
         // We processed current_p, but it might still be in the hash table.
         // The hash table nodes manage their own state copies.
         // The state popped from the heap needs freeing ONLY if it wasn't
         // the winning state and wasn't re-inserted via relaxation.
         // It's safer to free all allocated memory at the very end.
         // free(current_p); // This is risky due to hash table references
    }

    printf("%d\n", min_energy);

    // --- Cleanup ---
    // Free heap nodes (pointers to states)
     for (int i = 0; i < heap.size; ++i) {
         free(heap.data[i]);
     }
     // Free hash table nodes and their states
     for (int i = 0; i < HASH_TABLE_SIZE; ++i) {
         HashNode* current = hash_table[i];
         while (current != NULL) {
             HashNode* temp = current;
             current = current->next;
             // The state inside HashNode was copied, no separate free needed unless state itself contained pointers
             free(temp);
         }
     }
     // Note: The winning state 'current_p' points to memory managed by the hash table,
     // so it will be freed during hash table cleanup if it was inserted.
     // If the start state was the goal, its memory (`start_node`) also needs freeing if it wasn't overwritten in the hash table.
     // This simplified cleanup assumes all allocated states end up in the hash table.
}


int main() {
    solve();
    return 0;
}
