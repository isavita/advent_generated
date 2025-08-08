
#!/usr/bin/env bash
# ------------------------------------------------------------
#  Amphipod solver – Bash wrapper that compiles and runs the
#  original C implementation.
# ------------------------------------------------------------
set -euo pipefail

# --- 1. Write the C source --------------------------------
cat > amphipod.c <<'EOF'
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

#define ROWS 7
#define COLS 13
#define HEAP_MAX_SIZE 2000000
#define HASH_TABLE_SIZE 4000000

typedef struct {
    int r, c;
} Coord;

typedef struct {
    char grid[ROWS][COLS];
    long long energy_used;
} State;

// --- Priority Queue (Min-Heap) Implementation ---
State heap[HEAP_MAX_SIZE];
int heap_size = 0;

void swap_states(State *a, State *b) {
    State temp = *a;
    *a = *b;
    *b = temp;
}

void heap_push(State state) {
    if (heap_size >= HEAP_MAX_SIZE) {
        fprintf(stderr, "Heap overflow\n");
        exit(1);
    }
    heap[heap_size] = state;
    int current = heap_size++;
    while (current > 0) {
        int parent = (current - 1) / 2;
        if (heap[current].energy_used < heap[parent].energy_used) {
            swap_states(&heap[current], &heap[parent]);
            current = parent;
        } else {
            break;
        }
    }
}

State heap_pop() {
    if (heap_size <= 0) {
        fprintf(stderr, "Heap underflow\n");
        exit(1);
    }
    State top = heap[0];
    heap[0] = heap[--heap_size];
    int current = 0;
    while (true) {
        int left_child = 2 * current + 1;
        int right_child = 2 * current + 2;
        int smallest = current;

        if (left_child < heap_size && heap[left_child].energy_used < heap[smallest].energy_used) {
            smallest = left_child;
        }
        if (right_child < heap_size && heap[right_child].energy_used < heap[smallest].energy_used) {
            smallest = right_child;
        }

        if (smallest != current) {
            swap_states(&heap[current], &heap[smallest]);
            current = smallest;
        } else {
            break;
        }
    }
    return top;
}

// --- Hash Set for Visited States ---
typedef struct HashNode {
    State state;
    struct HashNode* next;
} HashNode;

HashNode* hash_table[HASH_TABLE_SIZE];

unsigned int hash_grid(const char grid[ROWS][COLS]) {
    unsigned int hash = 5381;
    for (int r = 0; r < ROWS; ++r) {
        for (int c = 0; c < COLS; ++c) {
            hash = ((hash << 5) + hash) + grid[r][c];
        }
    }
    return hash % HASH_TABLE_SIZE;
}

bool set_contains(const State* state) {
    unsigned int index = hash_grid(state->grid);
    HashNode* current = hash_table[index];
    while (current != NULL) {
        if (memcmp(current->state.grid, state->grid, sizeof(state->grid)) == 0) {
            return current->state.energy_used <= state->energy_used;
        }
        current = current->next;
    }
    return false;
}

void set_insert(const State* state) {
    unsigned int index = hash_grid(state->grid);
    HashNode* current = hash_table[index];
    while (current != NULL) {
        if (memcmp(current->state.grid, state->grid, sizeof(state->grid)) == 0) {
            if (state->energy_used < current->state.energy_used) {
                current->state.energy_used = state->energy_used;
            }
            return;
        }
        current = current->next;
    }
    HashNode* new_node = (HashNode*)malloc(sizeof(HashNode));
    if (!new_node) {
        perror("Failed to allocate hash node");
        exit(1);
    }
    new_node->state = *state;
    new_node->next = hash_table[index];
    hash_table[index] = new_node;
}

void free_hash_table() {
    for (int i = 0; i < HASH_TABLE_SIZE; ++i) {
        HashNode* current = hash_table[i];
        while (current != NULL) {
            HashNode* temp = current;
            current = current->next;
            free(temp);
        }
        hash_table[i] = NULL;
    }
}

// --- Amphipod Logic ---
char room_coord_to_want_char(int r, int c) {
    if (r >= 2 && r <= 5) {
        if (c == 3) return 'A';
        if (c == 5) return 'B';
        if (c == 7) return 'C';
        if (c == 9) return 'D';
    }
    return '.';
}

bool is_all_done(const State* state) {
    for (int r = 2; r <= 5; ++r) {
        for (int c = 3; c <= 9; c += 2) {
            if (state->grid[r][c] != room_coord_to_want_char(r, c)) {
                return false;
            }
        }
    }
    return true;
}

int get_unsettled_coords(const State* state, Coord unsettled[16]) {
    int count = 0;
    for (int c = 1; c < COLS - 1; ++c) {
        if (state->grid[1][c] >= 'A' && state->grid[1][c] <= 'D') {
            unsettled[count++] = (Coord){1, c};
        }
    }
    for (int c = 3; c <= 9; c += 2) {
        bool room_ok_below = true;
        for (int r = 5; r >= 2; --r) {
            char current_char = state->grid[r][c];
            char want_char = room_coord_to_want_char(r, c);
            if (current_char == '.') {
                room_ok_below = false;
            } else if (current_char != want_char) {
                unsettled[count++] = (Coord){r, c};
                room_ok_below = false;
            } else {
                if (!room_ok_below) {
                    unsettled[count++] = (Coord){r, c};
                }
            }
        }
    }
    return count;
}

long long calc_energy(char type, Coord start, Coord end) {
    long long base_energy;
    switch (type) {
        case 'A': base_energy = 1; break;
        case 'B': base_energy = 10; break;
        case 'C': base_energy = 100; break;
        case 'D': base_energy = 1000; break;
        default: return -1;
    }
    int dist = abs(start.r - end.r) + abs(start.c - end.c);
    if (start.r > 1 && end.r == 1) {
        dist = (start.r - 1) + abs(start.c - end.c);
    } else if (start.r == 1 && end.r > 1) {
        dist = (end.r - 1) + abs(start.c - end.c);
    } else if (start.r > 1 && end.r > 1) {
        dist = (start.r - 1) + (end.r - 1) + abs(start.c - end.c);
    }
    return base_energy * dist;
}

// Simple Queue for BFS
typedef struct {
    Coord pos;
    int dist;
} BFSNode;

BFSNode bfs_queue[ROWS * COLS];
int bfs_q_front = 0, bfs_q_rear = 0;

void bfs_q_push(BFSNode node) {
    bfs_queue[bfs_q_rear++] = node;
}
BFSNode bfs_q_pop() {
    return bfs_queue[bfs_q_front++];
}
bool bfs_q_empty() {
    return bfs_q_front == bfs_q_rear;
}
void bfs_q_reset() {
    bfs_q_front = bfs_q_rear = 0;
}

void find_and_process_moves(const State* current_state, Coord start_coord) {
    char amphipod_type = current_state->grid[start_coord.r][start_coord.c];
    bool started_in_hallway = (start_coord.r == 1);

    bfs_q_reset();
    bool visited[ROWS][COLS] = {false};

    bfs_q_push((BFSNode){start_coord, 0});
    visited[start_coord.r][start_coord.c] = true;

    int dr[] = {-1, 1, 0, 0};
    int dc[] = {0, 0, -1, 1};

    while (!bfs_q_empty()) {
        BFSNode current_bfs = bfs_q_pop();
        Coord current_pos = current_bfs.pos;

        if (memcmp(&current_pos, &start_coord, sizeof(Coord)) != 0) {
            char target_want_char = room_coord_to_want_char(current_pos.r, current_pos.c);

            if (started_in_hallway) {
                if (target_want_char == amphipod_type) {
                    bool room_valid = true;
                    for (int r = current_pos.r + 1; r <= 5; ++r) {
                        if (current_state->grid[r][current_pos.c] != amphipod_type) {
                            room_valid = false;
                            break;
                        }
                    }
                    if (room_valid) {
                        State next_state = *current_state;
                        next_state.grid[current_pos.r][current_pos.c] = amphipod_type;
                        next_state.grid[start_coord.r][start_coord.c] = '.';
                        next_state.energy_used += calc_energy(amphipod_type, start_coord, current_pos);

                        if (!set_contains(&next_state)) {
                            set_insert(&next_state);
                            heap_push(next_state);
                        }
                    }
                }
            } else {
                if (current_pos.r == 1) {
                    if (current_pos.c != 3 && current_pos.c != 5 && current_pos.c != 7 && current_pos.c != 9) {
                        State next_state = *current_state;
                        next_state.grid[current_pos.r][current_pos.c] = amphipod_type;
                        next_state.grid[start_coord.r][start_coord.c] = '.';
                        next_state.energy_used += calc_energy(amphipod_type, start_coord, current_pos);

                        if (!set_contains(&next_state)) {
                            set_insert(&next_state);
                            heap_push(next_state);
                        }
                    }
                } else if (target_want_char == amphipod_type) {
                    bool room_valid = true;
                    for (int r = current_pos.r + 1; r <= 5; ++r) {
                        if (current_state->grid[r][current_pos.c] != amphipod_type && current_state->grid[r][current_pos.c] != '.') {
                            room_valid = false;
                            break;
                        }
                    }
                    for (int r = current_pos.r + 1; r <= 5; ++r) {
                        if (current_state->grid[r][current_pos.c] == '.') {
                            room_valid = false;
                            break;
                        }
                    }
                    if (room_valid) {
                        State next_state = *current_state;
                        next_state.grid[current_pos.r][current_pos.c] = amphipod_type;
                        next_state.grid[start_coord.r][start_coord.c] = '.';
                        next_state.energy_used += calc_energy(amphipod_type, start_coord, current_pos);

                        if (!set_contains(&next_state)) {
                            set_insert(&next_state);
                            heap_push(next_state);
                        }
                    }
                }
            }
        }

        for (int i = 0; i < 4; ++i) {
            Coord next_pos = {current_pos.r + dr[i], current_pos.c + dc[i]};
            if (next_pos.r >= 0 && next_pos.r < ROWS && next_pos.c >= 0 && next_pos.c < COLS &&
                current_state->grid[next_pos.r][next_pos.c] == '.' && !visited[next_pos.r][next_pos.c]) {
                visited[next_pos.r][next_pos.c] = true;
                bfs_q_push((BFSNode){next_pos, current_bfs.dist + 1});
            }
        }
    }
}

int main() {
    FILE *infile = fopen("input.txt", "r");
    if (!infile) {
        perror("Error opening input.txt");
        return 1;
    }

    State start_state = { .energy_used = 0 };
    char buffer[COLS + 2];

    for (int i = 0; i < 5; ++i) {
        if (!fgets(buffer, sizeof(buffer), infile)) {
            fprintf(stderr, "Error reading line %d from input.txt\n", i + 1);
            fclose(infile);
            return 1;
        }
        int len = strlen(buffer);
        if (len > 0 && buffer[len-1] == '\n') buffer[--len] = '\0';
        memset(start_state.grid[i], ' ', COLS);
        memcpy(start_state.grid[i], buffer, len);
        start_state.grid[i][COLS-1] = '\0';
    }
    fclose(infile);

    memcpy(start_state.grid[5], start_state.grid[3], COLS);
    memcpy(start_state.grid[6], start_state.grid[4], COLS);

    memcpy(start_state.grid[3], "  #D#C#B#A#", 11);
    memset(start_state.grid[3] + 11, ' ', COLS - 11);
    memcpy(start_state.grid[4], "  #D#B#A#C#", 11);
    memset(start_state.grid[4] + 11, ' ', COLS - 11);

    for (int r = 0; r < ROWS; ++r) {
        for(int c=0; c < COLS; ++c) {
            if (r == 0 || r == ROWS -1 || c == 0 || c == COLS -1 || (r > 1 && (c < 1 || c > 11)) ) {
                if (start_state.grid[r][c] != '#') start_state.grid[r][c] = '#';
            }
            if (start_state.grid[r][c] == ' ') start_state.grid[r][c] = '#';
        }
    }
    for (int c=1; c <= 11; ++c) start_state.grid[1][c] = '.';
    for(int r=2; r<=5; ++r) {
        for(int c=3; c<=9; c+=2) {
            if (start_state.grid[r][c] < 'A' || start_state.grid[r][c] > 'D')
                start_state.grid[r][c] = '.';
        }
    }

    for (int i = 0; i < HASH_TABLE_SIZE; ++i) {
        hash_table[i] = NULL;
    }

    heap_push(start_state);
    set_insert(&start_state);

    long long final_energy = -1;

    while (heap_size > 0) {
        State current = heap_pop();

        if (is_all_done(&current)) {
            final_energy = current.energy_used;
            break;
        }

        Coord unsettled_coords[16];
        int unsettled_count = get_unsettled_coords(&current, unsettled_coords);

        for (int i = 0; i < unsettled_count; ++i) {
            find_and_process_moves(&current, unsettled_coords[i]);
        }
    }

    if (final_energy != -1) {
        printf("%lld\n", final_energy);
    } else {
        printf("No solution found\n");
    }

    free_hash_table();
    return 0;
}
EOF

# --- 2. Compile the C program --------------------------------
gcc -O2 -std=c11 -Wall -Wextra -o amphipod amphipod.c

# --- 3. Run the solver --------------------------------
./amphipod

# --- 4. Clean up --------------------------------
rm amphipod amphipod.c
