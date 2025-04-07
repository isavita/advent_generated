
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// --- Configuration ---
#define MEMORY_SIZE 8192      // Initial Intcode memory size (can grow)
#define MAP_DIM 50            // Dimension of the map grid (MAP_DIM x MAP_DIM)
#define MAP_CENTER (MAP_DIM / 2) // Center coordinates for starting position

// --- Constants ---
// Tile types
#define UNKNOWN ' '
#define EMPTY '.'
#define WALL '#'
#define OXYGEN 'O'
#define START 'S' // To mark the start after exploration

// Movement commands (Intcode input)
#define NORTH 1
#define SOUTH 2
#define WEST 3
#define EAST 4

// Status codes (Intcode output)
#define HIT_WALL 0
#define MOVED 1
#define FOUND_OXYGEN 2

// Intcode run status
#define STATUS_OK 0
#define STATUS_NEED_INPUT 1
#define STATUS_HALTED 2

// --- Data Structures ---

// Intcode Computer State
typedef struct {
    long long *mem;
    long long ip;
    long long relative_base;
    long long mem_capacity;
} IntcodeState;

// Point structure for coordinates
typedef struct {
    int x;
    int y;
} Point;

// Queue Item for BFS
typedef struct {
    Point pos;
    int distance; // Distance for Part 1, Time for Part 2
} QueueItem;

// Simple Queue Implementation (using a circular array)
#define QUEUE_CAPACITY (MAP_DIM * MAP_DIM) // Max possible items
typedef struct {
    QueueItem items[QUEUE_CAPACITY];
    int head;
    int tail;
    int count;
} Queue;

// Map Grid
char map[MAP_DIM][MAP_DIM];
Point oxygen_pos = {-1, -1};
Point start_pos = {MAP_CENTER, MAP_CENTER};

// Movement deltas (corresponds to NORTH, SOUTH, WEST, EAST)
int dx[] = {0, 0, 0, -1, 1}; // Index 0 unused
int dy[] = {0, -1, 1, 0, 0}; // Index 0 unused

// Opposite moves for backtracking
int opposite_move[] = {0, SOUTH, NORTH, EAST, WEST}; // Index 0 unused

// --- Queue Functions ---

void init_queue(Queue *q) {
    q->head = 0;
    q->tail = 0;
    q->count = 0;
}

int is_queue_empty(Queue *q) {
    return q->count == 0;
}

int is_queue_full(Queue *q) {
    return q->count == QUEUE_CAPACITY;
}

void enqueue(Queue *q, QueueItem item) {
    if (!is_queue_full(q)) {
        q->items[q->tail] = item;
        q->tail = (q->tail + 1) % QUEUE_CAPACITY;
        q->count++;
    } else {
        fprintf(stderr, "Error: Queue overflow\n");
        // In a real scenario, might reallocate or handle differently
    }
}

QueueItem dequeue(Queue *q) {
    QueueItem item = {{-1, -1}, -1}; // Default invalid item
    if (!is_queue_empty(q)) {
        item = q->items[q->head];
        q->head = (q->head + 1) % QUEUE_CAPACITY;
        q->count--;
    } else {
        fprintf(stderr, "Error: Queue underflow\n");
    }
    return item;
}

// --- Intcode Functions ---

// Ensure memory is large enough, reallocating if necessary
void ensure_memory(IntcodeState *state, long long address) {
    if (address >= state->mem_capacity) {
        long long new_capacity = state->mem_capacity;
        while (address >= new_capacity) {
            new_capacity *= 2; // Double the capacity
        }
        long long *new_mem = (long long *)realloc(state->mem, new_capacity * sizeof(long long));
        if (new_mem == NULL) {
            perror("Failed to reallocate Intcode memory");
            free(state->mem);
            exit(EXIT_FAILURE);
        }
        // Zero out the newly allocated memory
        memset(new_mem + state->mem_capacity, 0, (new_capacity - state->mem_capacity) * sizeof(long long));
        state->mem = new_mem;
        state->mem_capacity = new_capacity;
    }
}

// Get value based on parameter mode
long long get_param(IntcodeState *state, long long offset, int mode) {
    long long param_addr = state->ip + offset;
    ensure_memory(state, param_addr); // Ensure instruction pointer + offset is valid
    long long param = state->mem[param_addr];
    long long value_addr;

    switch (mode) {
        case 0: // Position Mode
            value_addr = param;
            break;
        case 1: // Immediate Mode
            return param; // The parameter itself is the value
        case 2: // Relative Mode
            value_addr = state->relative_base + param;
            break;
        default:
            fprintf(stderr, "Error: Invalid parameter mode %d at ip %lld\n", mode, state->ip);
            exit(EXIT_FAILURE);
    }
    ensure_memory(state, value_addr); // Ensure target address is valid
    return state->mem[value_addr];
}

// Get address based on parameter mode (for writing)
long long get_addr(IntcodeState *state, long long offset, int mode) {
    long long param_addr = state->ip + offset;
    ensure_memory(state, param_addr); // Ensure instruction pointer + offset is valid
    long long param = state->mem[param_addr];
    long long write_addr;

    switch (mode) {
        case 0: // Position Mode
            write_addr = param;
            break;
        case 2: // Relative Mode
            write_addr = state->relative_base + param;
            break;
        // Mode 1 (Immediate) is invalid for writing addresses
        default:
            fprintf(stderr, "Error: Invalid address mode %d for writing at ip %lld\n", mode, state->ip);
            exit(EXIT_FAILURE);
    }
    ensure_memory(state, write_addr); // Ensure target address is valid before returning
    return write_addr;
}

// Run the Intcode program
int run_intcode(IntcodeState *state, long long input, long long *output) {
    while (1) {
        ensure_memory(state, state->ip); // Ensure current instruction pointer is valid
        long long opcode_full = state->mem[state->ip];
        int opcode = opcode_full % 100;
        int mode1 = (opcode_full / 100) % 10;
        int mode2 = (opcode_full / 1000) % 10;
        int mode3 = (opcode_full / 10000) % 10;

        long long val1, val2, addr;

        switch (opcode) {
            case 1: // Add
                val1 = get_param(state, 1, mode1);
                val2 = get_param(state, 2, mode2);
                addr = get_addr(state, 3, mode3);
                state->mem[addr] = val1 + val2;
                state->ip += 4;
                break;
            case 2: // Multiply
                val1 = get_param(state, 1, mode1);
                val2 = get_param(state, 2, mode2);
                addr = get_addr(state, 3, mode3);
                state->mem[addr] = val1 * val2;
                state->ip += 4;
                break;
            case 3: // Input
                addr = get_addr(state, 1, mode1);
                state->mem[addr] = input;
                state->ip += 2;
                // Input consumed, return to get next input potentially
                // However, for the robot, we expect an output after input
                // so we continue execution until output or halt.
                break;
            case 4: // Output
                *output = get_param(state, 1, mode1);
                state->ip += 2;
                return STATUS_OK; // Return with output
            case 5: // Jump-if-true
                val1 = get_param(state, 1, mode1);
                val2 = get_param(state, 2, mode2);
                if (val1 != 0) {
                    state->ip = val2;
                } else {
                    state->ip += 3;
                }
                break;
            case 6: // Jump-if-false
                val1 = get_param(state, 1, mode1);
                val2 = get_param(state, 2, mode2);
                if (val1 == 0) {
                    state->ip = val2;
                } else {
                    state->ip += 3;
                }
                break;
            case 7: // Less than
                val1 = get_param(state, 1, mode1);
                val2 = get_param(state, 2, mode2);
                addr = get_addr(state, 3, mode3);
                state->mem[addr] = (val1 < val2) ? 1 : 0;
                state->ip += 4;
                break;
            case 8: // Equals
                val1 = get_param(state, 1, mode1);
                val2 = get_param(state, 2, mode2);
                addr = get_addr(state, 3, mode3);
                state->mem[addr] = (val1 == val2) ? 1 : 0;
                state->ip += 4;
                break;
            case 9: // Adjust relative base
                val1 = get_param(state, 1, mode1);
                state->relative_base += val1;
                state->ip += 2;
                break;
            case 99: // Halt
                return STATUS_HALTED;
            default:
                fprintf(stderr, "Error: Unknown opcode %d at ip %lld\n", opcode, state->ip);
                exit(EXIT_FAILURE);
        }
    }
    // Should not be reached unless run_intcode is modified to need input mid-run
    // return STATUS_NEED_INPUT;
}

// --- Exploration (DFS) ---

void explore(IntcodeState *state, int x, int y) {
    // Try moving in all four directions
    for (int move_cmd = NORTH; move_cmd <= EAST; ++move_cmd) {
        int next_x = x + dx[move_cmd];
        int next_y = y + dy[move_cmd];

        // Check bounds (although map is large, good practice)
        if (next_x < 0 || next_x >= MAP_DIM || next_y < 0 || next_y >= MAP_DIM) {
            continue;
        }

        // Only explore unknown areas
        if (map[next_y][next_x] == UNKNOWN) {
            long long status_code;
            int run_status = run_intcode(state, move_cmd, &status_code);

            if (run_status == STATUS_HALTED) {
                fprintf(stderr, "Error: Intcode halted unexpectedly during exploration.\n");
                return; // Or exit
            }

            switch (status_code) {
                case HIT_WALL:
                    map[next_y][next_x] = WALL;
                    break;
                case MOVED:
                    map[next_y][next_x] = EMPTY;
                    explore(state, next_x, next_y); // Explore from the new position

                    // Backtrack: Move back to the previous position (x, y)
                    long long backtrack_status;
                    run_intcode(state, opposite_move[move_cmd], &backtrack_status);
                    // We assume backtracking always succeeds if the forward move did
                    if (backtrack_status != MOVED) {
                       fprintf(stderr, "Error: Backtrack failed! Status: %lld\n", backtrack_status);
                       // Potentially handle this error more gracefully
                    }
                    break;
                case FOUND_OXYGEN:
                    map[next_y][next_x] = OXYGEN;
                    oxygen_pos.x = next_x;
                    oxygen_pos.y = next_y;
                    explore(state, next_x, next_y); // Explore from the oxygen system position

                    // Backtrack after exploring from oxygen location
                    long long backtrack_oxygen_status;
                     run_intcode(state, opposite_move[move_cmd], &backtrack_oxygen_status);
                    if (backtrack_oxygen_status != MOVED) {
                       fprintf(stderr, "Error: Backtrack from oxygen failed! Status: %lld\n", backtrack_oxygen_status);
                    }
                    break;
                default:
                    fprintf(stderr, "Error: Unknown status code %lld\n", status_code);
                    break;
            }
        }
    }
}

// --- Pathfinding & Oxygen Fill (BFS) ---

int bfs(Point start, Point target, int *part2_time) {
    Queue q;
    init_queue(&q);
    int distance[MAP_DIM][MAP_DIM];
    memset(distance, -1, sizeof(distance)); // -1 means not visited

    QueueItem start_item = {start, 0};
    enqueue(&q, start_item);
    distance[start.y][start.x] = 0;

    int shortest_path = -1;
    int max_time = 0; // For Part 2

    while (!is_queue_empty(&q)) {
        QueueItem current = dequeue(&q);
        Point pos = current.pos;
        int dist = current.distance;

        // Update max time for Part 2 (oxygen fill)
        if (dist > max_time) {
            max_time = dist;
        }

        // Check if we reached the target (for Part 1)
        if (pos.x == target.x && pos.y == target.y) {
            shortest_path = dist;
            // For Part 1, we could stop here, but we continue for Part 2
            // to find the max_time by exploring the whole reachable area.
        }

        // Explore neighbors
        for (int move_cmd = NORTH; move_cmd <= EAST; ++move_cmd) {
            int next_x = pos.x + dx[move_cmd];
            int next_y = pos.y + dy[move_cmd];

            // Check bounds and if traversable (not WALL) and not visited
             if (next_x >= 0 && next_x < MAP_DIM && next_y >= 0 && next_y < MAP_DIM &&
                 map[next_y][next_x] != WALL && distance[next_y][next_x] == -1)
             {
                distance[next_y][next_x] = dist + 1;
                QueueItem next_item = {{next_x, next_y}, dist + 1};
                enqueue(&q, next_item);
            }
        }
    }

    *part2_time = max_time; // Return max_time via pointer
    return shortest_path; // Return shortest path to oxygen
}

// --- Main Function ---

int main() {
    // --- Initialization ---
    IntcodeState state = {0};
    state.mem = (long long *)calloc(MEMORY_SIZE, sizeof(long long));
    state.ip = 0;
    state.relative_base = 0;
    state.mem_capacity = MEMORY_SIZE;

    if (state.mem == NULL) {
        perror("Failed to allocate initial Intcode memory");
        return EXIT_FAILURE;
    }

    // Initialize map
    for (int y = 0; y < MAP_DIM; ++y) {
        for (int x = 0; x < MAP_DIM; ++x) {
            map[y][x] = UNKNOWN;
        }
    }
    map[start_pos.y][start_pos.x] = EMPTY; // Start position is known empty

    // --- Read Input File ---
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening input.txt");
        free(state.mem);
        return EXIT_FAILURE;
    }

    long long value;
    long long index = 0;
    while (fscanf(file, "%lld,", &value) == 1) {
        ensure_memory(&state, index); // Make sure there's space
        state.mem[index++] = value;
    }
    fclose(file);

    // --- Phase 1: Explore the map using DFS ---
    explore(&state, start_pos.x, start_pos.y);

    // Mark start position visually after exploration is done
    if (start_pos.x == oxygen_pos.x && start_pos.y == oxygen_pos.y) {
         map[start_pos.y][start_pos.x] = OXYGEN; // Should not happen based on puzzle
    } else {
         map[start_pos.y][start_pos.x] = START;
    }


    if (oxygen_pos.x == -1) {
        fprintf(stderr, "Error: Oxygen System not found during exploration!\n");
        free(state.mem);
        return EXIT_FAILURE;
    }

    // --- Phase 2: Find shortest path using BFS (Part 1) ---
    // We run BFS starting from the *start* position to the oxygen system.
    int part2_dummy_time; // We don't need the max time from this run
    int part1_result = bfs(start_pos, oxygen_pos, &part2_dummy_time);


    // --- Phase 3: Calculate oxygen fill time using BFS (Part 2) ---
    // We run BFS starting from the *oxygen* position to find the max time.
    int part2_result;
    // The target point for this BFS doesn't matter, we just need the max time.
    // We can use the start_pos as a dummy target.
    bfs(oxygen_pos, start_pos, &part2_result);


    // --- Output Results ---
    printf("Part 1: Fewest movements to reach oxygen system: %d\n", part1_result);
    printf("Part 2: Minutes to fill with oxygen: %d\n", part2_result);

    // --- Cleanup ---
    free(state.mem);

    return EXIT_SUCCESS;
}
