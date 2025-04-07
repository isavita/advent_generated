
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

// --- Constants ---
#define MEM_SIZE 8192 // Max Intcode memory size
#define MAP_DIM 101  // Dimension for the map grid (odd number for centering)
#define MAP_OFFSET (MAP_DIM / 2) // Offset to center coordinates (0,0) in the array
#define MAX_QUEUE (MAP_DIM * MAP_DIM * 2) // Max size for BFS queue

// Map tile types
#define UNKNOWN -1
#define WALL 0
#define FLOOR 1
#define OXYGEN 2
#define START 3 // Just for marking the start visually if needed

// Intcode run results
#define RUN_OK 0
#define RUN_INPUT_NEEDED 1
#define RUN_OUTPUT_READY 2
#define RUN_HALTED 3

// Movement commands (match problem statement)
#define NORTH 1
#define SOUTH 2
#define WEST 3
#define EAST 4

// --- Data Structures ---

// Intcode Computer State
typedef struct {
    long long mem[MEM_SIZE];
    long long ip;
    long long relative_base;
    int halted;
    long long input_val;
    long long output_val;
} IntcodeState;

// BFS Queue State
typedef struct {
    int x;
    int y;
    int dist;
    IntcodeState intcode; // Store a copy of the intcode state for this path
} BFSState;

// BFS Queue
typedef struct {
    BFSState items[MAX_QUEUE];
    int head;
    int tail;
    int count;
} Queue;

// --- Global Variables ---
int map[MAP_DIM][MAP_DIM];
int min_dist[MAP_DIM][MAP_DIM];
Queue bfs_queue;
int oxygen_x = -1, oxygen_y = -1;
int final_distance = -1;

// --- Queue Operations ---
void init_queue() {
    bfs_queue.head = 0;
    bfs_queue.tail = 0;
    bfs_queue.count = 0;
}

int is_queue_empty() {
    return bfs_queue.count == 0;
}

int is_queue_full() {
    return bfs_queue.count == MAX_QUEUE;
}

void enqueue(BFSState state) {
    if (is_queue_full()) {
        fprintf(stderr, "Error: BFS Queue overflow\n");
        exit(EXIT_FAILURE);
    }
    bfs_queue.items[bfs_queue.tail] = state; // Copies the state
    bfs_queue.tail = (bfs_queue.tail + 1) % MAX_QUEUE;
    bfs_queue.count++;
}

BFSState dequeue() {
    if (is_queue_empty()) {
        fprintf(stderr, "Error: BFS Queue underflow\n");
        exit(EXIT_FAILURE);
    }
    BFSState state = bfs_queue.items[bfs_queue.head];
    bfs_queue.head = (bfs_queue.head + 1) % MAX_QUEUE;
    bfs_queue.count--;
    return state;
}

// --- Intcode Functions ---

// Gets the value of a parameter based on its mode
long long get_param_val(IntcodeState *state, int mode, long long param) {
    switch (mode) {
        case 0: // Position mode
            if (param < 0 || param >= MEM_SIZE) { fprintf(stderr, "Error: Position mode OOB read (%lld)\n", param); exit(1); }
            return state->mem[param];
        case 1: // Immediate mode
            return param;
        case 2: // Relative mode
             if (state->relative_base + param < 0 || state->relative_base + param >= MEM_SIZE) { fprintf(stderr, "Error: Relative mode OOB read (%lld)\n", state->relative_base + param); exit(1); }
           return state->mem[state->relative_base + param];
        default:
            fprintf(stderr, "Error: Invalid parameter mode %d\n", mode);
            exit(EXIT_FAILURE);
    }
}

// Gets the address where a result should be stored based on its mode
long long get_param_addr(IntcodeState *state, int mode, long long param) {
     if (param < 0) { fprintf(stderr, "Error: Negative address param (%lld)\n", param); exit(1); }
    switch (mode) {
        case 0: // Position mode
            if (param >= MEM_SIZE) { fprintf(stderr, "Error: Position mode OOB write addr (%lld)\n", param); exit(1); }
            return param;
        case 2: // Relative mode
             if (state->relative_base + param < 0 || state->relative_base + param >= MEM_SIZE) { fprintf(stderr, "Error: Relative mode OOB write addr (%lld)\n", state->relative_base + param); exit(1); }
            return state->relative_base + param;
        default:
            // Mode 1 is invalid for write addresses
            fprintf(stderr, "Error: Invalid parameter mode %d for write address\n", mode);
            exit(EXIT_FAILURE);
    }
}


// Runs the Intcode program until it needs input, produces output, or halts
int run_intcode(IntcodeState *state) {
    if (state->halted) {
        return RUN_HALTED;
    }

    while (state->ip >= 0 && state->ip < MEM_SIZE) {
        long long instruction = state->mem[state->ip];
        int opcode = instruction % 100;
        int mode1 = (instruction / 100) % 10;
        int mode2 = (instruction / 1000) % 10;
        int mode3 = (instruction / 10000) % 10;

        long long p1, p2, p3, addr;

        switch (opcode) {
            case 1: // Add
                if (state->ip + 3 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (add)\n"); exit(1); }
                p1 = get_param_val(state, mode1, state->mem[state->ip + 1]);
                p2 = get_param_val(state, mode2, state->mem[state->ip + 2]);
                addr = get_param_addr(state, mode3, state->mem[state->ip + 3]);
                state->mem[addr] = p1 + p2;
                state->ip += 4;
                break;
            case 2: // Multiply
                if (state->ip + 3 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (mul)\n"); exit(1); }
                p1 = get_param_val(state, mode1, state->mem[state->ip + 1]);
                p2 = get_param_val(state, mode2, state->mem[state->ip + 2]);
                addr = get_param_addr(state, mode3, state->mem[state->ip + 3]);
                state->mem[addr] = p1 * p2;
                state->ip += 4;
                break;
            case 3: // Input
                 if (state->ip + 1 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (in)\n"); exit(1); }
                addr = get_param_addr(state, mode1, state->mem[state->ip + 1]);
                // Signal that input is needed
                state->ip += 2; // Advance IP *before* returning
                return RUN_INPUT_NEEDED;
                // Input value will be written *after* function returns and input is provided
                // state->mem[addr] = state->input_val; // Done outside
                // state->ip += 2; // Moved up
                // break; // Not reached
            case 4: // Output
                 if (state->ip + 1 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (out)\n"); exit(1); }
                p1 = get_param_val(state, mode1, state->mem[state->ip + 1]);
                state->output_val = p1;
                state->ip += 2;
                return RUN_OUTPUT_READY; // Signal output is ready
            case 5: // Jump-if-true
                 if (state->ip + 2 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (jt)\n"); exit(1); }
                p1 = get_param_val(state, mode1, state->mem[state->ip + 1]);
                p2 = get_param_val(state, mode2, state->mem[state->ip + 2]);
                if (p1 != 0) {
                    state->ip = p2;
                } else {
                    state->ip += 3;
                }
                break;
            case 6: // Jump-if-false
                 if (state->ip + 2 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (jf)\n"); exit(1); }
                p1 = get_param_val(state, mode1, state->mem[state->ip + 1]);
                p2 = get_param_val(state, mode2, state->mem[state->ip + 2]);
                if (p1 == 0) {
                    state->ip = p2;
                } else {
                    state->ip += 3;
                }
                break;
            case 7: // Less than
                 if (state->ip + 3 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (lt)\n"); exit(1); }
                p1 = get_param_val(state, mode1, state->mem[state->ip + 1]);
                p2 = get_param_val(state, mode2, state->mem[state->ip + 2]);
                addr = get_param_addr(state, mode3, state->mem[state->ip + 3]);
                state->mem[addr] = (p1 < p2) ? 1 : 0;
                state->ip += 4;
                break;
            case 8: // Equals
                 if (state->ip + 3 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (eq)\n"); exit(1); }
                p1 = get_param_val(state, mode1, state->mem[state->ip + 1]);
                p2 = get_param_val(state, mode2, state->mem[state->ip + 2]);
                addr = get_param_addr(state, mode3, state->mem[state->ip + 3]);
                state->mem[addr] = (p1 == p2) ? 1 : 0;
                state->ip += 4;
                break;
            case 9: // Adjust relative base
                 if (state->ip + 1 >= MEM_SIZE) { fprintf(stderr, "Error: OOB instruction read (rel)\n"); exit(1); }
                p1 = get_param_val(state, mode1, state->mem[state->ip + 1]);
                state->relative_base += p1;
                state->ip += 2;
                break;
            case 99: // Halt
                state->halted = 1;
                return RUN_HALTED;
            default:
                fprintf(stderr, "Error: Unknown opcode %d at ip %lld\n", opcode, state->ip);
                exit(EXIT_FAILURE);
        }
    }
     fprintf(stderr, "Error: Instruction pointer out of bounds (%lld)\n", state->ip);
     exit(1);
    // return RUN_OK; // Should not be reached if loop condition is correct
}

// Helper to provide input after RUN_INPUT_NEEDED
void provide_input(IntcodeState *state, long long input) {
    // The address calculation was done *before* run_intcode returned RUN_INPUT_NEEDED
    // The IP was also advanced by 2 already. We need the instruction *before* the IP jump.
    long long instruction = state->mem[state->ip - 2]; // Get the input instruction
    int mode1 = (instruction / 100) % 10;
    long long addr_param = state->mem[state->ip - 1]; // Get the parameter for the address
    long long addr = get_param_addr(state, mode1, addr_param);
    state->mem[addr] = input;
}


// --- Map and BFS Logic ---

void init_map() {
    for (int i = 0; i < MAP_DIM; ++i) {
        for (int j = 0; j < MAP_DIM; ++j) {
            map[i][j] = UNKNOWN;
            min_dist[i][j] = INT_MAX;
        }
    }
}

void explore() {
    init_queue();
    init_map();

    // Initial Intcode state (will be copied from the parsed input)
    IntcodeState initial_intcode_state = { {0}, 0, 0, 0, 0, 0 };

    // Load program from file
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        exit(EXIT_FAILURE);
    }

    long long val;
    int index = 0;
    while (fscanf(file, "%lld,", &val) == 1 && index < MEM_SIZE) {
        initial_intcode_state.mem[index++] = val;
    }
    fclose(file);

    // Starting position
    int start_x = MAP_OFFSET;
    int start_y = MAP_OFFSET;

    map[start_y][start_x] = FLOOR; // Starting point is always floor
    min_dist[start_y][start_x] = 0;

    // Initial BFS state
    BFSState initial_bfs_state;
    initial_bfs_state.x = start_x;
    initial_bfs_state.y = start_y;
    initial_bfs_state.dist = 0;
    initial_bfs_state.intcode = initial_intcode_state; // Copy initial state

    enqueue(initial_bfs_state);

    int dx[] = {0, 0, -1, 1}; // Corresponds to N(1), S(2), W(3), E(4) -> indices 0, 1, 2, 3
    int dy[] = {-1, 1, 0, 0}; // Screen coordinates: Y decreases going North

    while (!is_queue_empty()) {
        BFSState current = dequeue();

        // Try moving in all 4 directions
        for (int move_cmd = 1; move_cmd <= 4; ++move_cmd) {
            int next_x = current.x + dx[move_cmd - 1];
            int next_y = current.y + dy[move_cmd - 1];

            // Basic bounds check (although map is large)
            if (next_x < 0 || next_x >= MAP_DIM || next_y < 0 || next_y >= MAP_DIM) {
                continue;
            }

            // Create a *copy* of the Intcode state for this specific move attempt
            IntcodeState next_intcode_state = current.intcode;

            // Run the Intcode computer, providing the move command
            int run_status = run_intcode(&next_intcode_state);

            if (run_status == RUN_INPUT_NEEDED) {
                provide_input(&next_intcode_state, (long long)move_cmd);
                run_status = run_intcode(&next_intcode_state); // Continue running
            } else {
                 fprintf(stderr, "Error: Intcode did not request input when expected.\n");
                 exit(1);
            }


            if (run_status == RUN_OUTPUT_READY) {
                int status_code = (int)next_intcode_state.output_val;
                int next_dist = current.dist + 1;

                switch (status_code) {
                    case WALL:
                        map[next_y][next_x] = WALL;
                        // Do not enqueue, droid didn't move
                        break;

                    case FLOOR:
                    case OXYGEN: // Treat OXYGEN like FLOOR for pathfinding, but record location/distance
                        // Only explore if this path is shorter or the first time visiting
                        if (next_dist < min_dist[next_y][next_x]) {
                             map[next_y][next_x] = (status_code == OXYGEN) ? OXYGEN : FLOOR;
                             min_dist[next_y][next_x] = next_dist;

                             if (status_code == OXYGEN) {
                                 oxygen_x = next_x;
                                 oxygen_y = next_y;
                                 // We found the oxygen system via the shortest path!
                                 final_distance = next_dist;
                                 // Clear the queue to stop BFS (optional, slightly faster)
                                 // init_queue(); // Commented out to allow full map exploration if needed later
                                 printf("%d\n", final_distance); // Print the result
                                 return; // Exit the explore function
                             }

                             // Enqueue the new state for further exploration
                             BFSState next_bfs_state;
                             next_bfs_state.x = next_x;
                             next_bfs_state.y = next_y;
                             next_bfs_state.dist = next_dist;
                             next_bfs_state.intcode = next_intcode_state; // Copy the *modified* intcode state
                             enqueue(next_bfs_state);
                        }
                        break;

                    default:
                        fprintf(stderr, "Error: Unknown status code %d received\n", status_code);
                        exit(EXIT_FAILURE);
                }

            } else if (run_status == RUN_HALTED) {
                 fprintf(stderr, "Warning: Intcode halted unexpectedly during exploration.\n");
                 // Potentially handle this, maybe the map is fully explored or an error state
            } else {
                 fprintf(stderr, "Error: Unexpected Intcode run status %d after providing input.\n", run_status);
                 exit(1);
            }
        } // end for each move
    } // end while queue not empty

    // If the loop finishes and final_distance is still -1, oxygen wasn't found
    if (final_distance == -1) {
         fprintf(stderr, "Error: Oxygen system not found.\n");
         exit(EXIT_FAILURE);
    }
}

// --- Main Function ---
int main() {
    explore();
    // The result is printed inside explore() when the oxygen system is found.
    return 0;
}
