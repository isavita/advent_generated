
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_MEM_SIZE 10000
#define MAX_GRID_DIM 100
#define MAX_PATH_LEN 1000
#define MAX_FUNC_LEN 21 // 20 chars + null terminator
#define MAX_TOKENS 500

typedef long long ll;

// --- Queue Implementation ---
typedef struct {
    ll* data;
    size_t capacity;
    size_t count;
    size_t head;
} Queue;

void init_queue(Queue* q, size_t initial_capacity) {
    q->data = malloc(initial_capacity * sizeof(ll));
    if (!q->data) { perror("Failed to allocate queue data"); exit(EXIT_FAILURE); }
    q->capacity = initial_capacity;
    q->count = 0;
    q->head = 0;
}

void free_queue(Queue* q) {
    free(q->data);
    q->data = NULL;
    q->capacity = 0;
    q->count = 0;
    q->head = 0;
}

void ensure_queue_capacity(Queue* q, size_t required) {
    if (q->capacity < required) {
        size_t new_capacity = q->capacity == 0 ? 1 : q->capacity;
        while (new_capacity < required) {
            new_capacity *= 2;
        }
        ll* new_data = realloc(q->data, new_capacity * sizeof(ll));
        if (!new_data) { perror("Failed to realloc queue data"); exit(EXIT_FAILURE); }
        q->data = new_data;
        q->capacity = new_capacity;
    }
}

void enqueue(Queue* q, ll value) {
    ensure_queue_capacity(q, q->count + 1);
    size_t tail_index = (q->head + q->count) % q->capacity;
     // Handle wrap-around if needed for circular buffer (not strictly needed here as we resize)
    if (q->count > 0 && tail_index < q->head && q->head + q->count >= q->capacity) {
         // Make space at the end if wrapping around fully filled buffer
         ensure_queue_capacity(q, q->count + 1);
         tail_index = q->count; // Should append at the end after realloc
    }

    q->data[tail_index] = value;
    q->count++;
}


ll dequeue(Queue* q) {
    if (q->count == 0) {
        fprintf(stderr, "Queue underflow\n");
        exit(EXIT_FAILURE);
    }
    ll value = q->data[q->head];
    q->head = (q->head + 1) % q->capacity;
    q->count--;
    return value;
}

void queue_to_array(Queue* q, ll* arr) {
    for(size_t i = 0; i < q->count; ++i) {
        arr[i] = q->data[(q->head + i) % q->capacity];
    }
}


// --- Intcode Computer ---
typedef struct {
    ll* memory;
    size_t mem_capacity;
    ll pointer;
    ll relative_base;
    Queue inputs;
    Queue outputs;
    int halted;
    int waiting_for_input;
} IntcodeComputer;

void ensure_mem_capacity(IntcodeComputer* comp, size_t addr) {
    if (addr >= comp->mem_capacity) {
        size_t new_capacity = comp->mem_capacity == 0 ? 1 : comp->mem_capacity;
        while (new_capacity <= addr) {
            new_capacity *= 2;
             if (new_capacity == 0) new_capacity = addr + 1; // Prevent infinite loop if initial capacity was 0 and addr is 0
        }
         // Ensure a minimum practical size
        if (new_capacity < 1024) new_capacity = 1024;


        ll* new_mem = realloc(comp->memory, new_capacity * sizeof(ll));
        if (!new_mem) { perror("Failed to realloc Intcode memory"); exit(EXIT_FAILURE); }

        // Zero out the newly allocated memory
        memset(new_mem + comp->mem_capacity, 0, (new_capacity - comp->mem_capacity) * sizeof(ll));

        comp->memory = new_mem;
        comp->mem_capacity = new_capacity;
    }
}

ll get_mem(IntcodeComputer* comp, ll addr) {
    if (addr < 0) { fprintf(stderr, "Invalid memory address: %lld\n", addr); exit(EXIT_FAILURE); }
    ensure_mem_capacity(comp, (size_t)addr);
    return comp->memory[addr];
}

void set_mem(IntcodeComputer* comp, ll addr, ll value) {
    if (addr < 0) { fprintf(stderr, "Invalid memory address for write: %lld\n", addr); exit(EXIT_FAILURE); }
    ensure_mem_capacity(comp, (size_t)addr);
    comp->memory[addr] = value;
}

ll get_param(IntcodeComputer* comp, int mode, ll param_addr) {
    ll raw_param = get_mem(comp, param_addr);
    switch (mode) {
        case 0: return get_mem(comp, raw_param);          // Position mode
        case 1: return raw_param;                         // Immediate mode
        case 2: return get_mem(comp, comp->relative_base + raw_param); // Relative mode
        default: fprintf(stderr, "Unknown parameter mode: %d\n", mode); exit(EXIT_FAILURE);
    }
}

void set_param(IntcodeComputer* comp, int mode, ll param_addr, ll value) {
    ll raw_param = get_mem(comp, param_addr);
    switch (mode) {
        case 0: set_mem(comp, raw_param, value); break;                     // Position mode
        case 2: set_mem(comp, comp->relative_base + raw_param, value); break; // Relative mode
        default: fprintf(stderr, "Unknown parameter mode for write: %d\n", mode); exit(EXIT_FAILURE);
    }
}

void init_computer(IntcodeComputer* comp, ll* program, size_t program_size) {
    comp->mem_capacity = program_size > MAX_MEM_SIZE ? program_size : MAX_MEM_SIZE;
    comp->memory = calloc(comp->mem_capacity, sizeof(ll));
     if (!comp->memory) { perror("Failed to allocate initial Intcode memory"); exit(EXIT_FAILURE); }
    memcpy(comp->memory, program, program_size * sizeof(ll));
    comp->pointer = 0;
    comp->relative_base = 0;
    init_queue(&comp->inputs, 10);
    init_queue(&comp->outputs, 100); // Initial capacity for outputs
    comp->halted = 0;
    comp->waiting_for_input = 0;
}

void free_computer(IntcodeComputer* comp) {
    free(comp->memory);
    free_queue(&comp->inputs);
    free_queue(&comp->outputs);
    comp->memory = NULL; // Prevent double free
}

void run_computer(IntcodeComputer* comp) {
    comp->waiting_for_input = 0;
    while (!comp->halted && !comp->waiting_for_input) {
        ll instruction = get_mem(comp, comp->pointer);
        int opcode = instruction % 100;
        int modes[3] = { (int)(instruction / 100) % 10, (int)(instruction / 1000) % 10, (int)(instruction / 10000) % 10 };
        ll p1, p2, p3;

        switch (opcode) {
            case 1: // Add
                p1 = get_param(comp, modes[0], comp->pointer + 1);
                p2 = get_param(comp, modes[1], comp->pointer + 2);
                set_param(comp, modes[2], comp->pointer + 3, p1 + p2);
                comp->pointer += 4;
                break;
            case 2: // Multiply
                p1 = get_param(comp, modes[0], comp->pointer + 1);
                p2 = get_param(comp, modes[1], comp->pointer + 2);
                set_param(comp, modes[2], comp->pointer + 3, p1 * p2);
                comp->pointer += 4;
                break;
            case 3: // Input
                if (comp->inputs.count == 0) {
                    comp->waiting_for_input = 1; // Signal wait
                    return; // Pause execution
                }
                ll input_val = dequeue(&comp->inputs);
                set_param(comp, modes[0], comp->pointer + 1, input_val);
                comp->pointer += 2;
                break;
            case 4: // Output
                p1 = get_param(comp, modes[0], comp->pointer + 1);
                enqueue(&comp->outputs, p1);
                comp->pointer += 2;
                break;
            case 5: // Jump-if-true
                p1 = get_param(comp, modes[0], comp->pointer + 1);
                p2 = get_param(comp, modes[1], comp->pointer + 2);
                if (p1 != 0) comp->pointer = p2;
                else comp->pointer += 3;
                break;
            case 6: // Jump-if-false
                p1 = get_param(comp, modes[0], comp->pointer + 1);
                p2 = get_param(comp, modes[1], comp->pointer + 2);
                if (p1 == 0) comp->pointer = p2;
                else comp->pointer += 3;
                break;
            case 7: // Less than
                p1 = get_param(comp, modes[0], comp->pointer + 1);
                p2 = get_param(comp, modes[1], comp->pointer + 2);
                set_param(comp, modes[2], comp->pointer + 3, p1 < p2);
                comp->pointer += 4;
                break;
            case 8: // Equals
                p1 = get_param(comp, modes[0], comp->pointer + 1);
                p2 = get_param(comp, modes[1], comp->pointer + 2);
                set_param(comp, modes[2], comp->pointer + 3, p1 == p2);
                comp->pointer += 4;
                break;
            case 9: // Adjust relative base
                p1 = get_param(comp, modes[0], comp->pointer + 1);
                comp->relative_base += p1;
                comp->pointer += 2;
                break;
            case 99: // Halt
                comp->halted = 1;
                break;
            default:
                fprintf(stderr, "Unknown opcode: %d at pointer %lld\n", opcode, comp->pointer);
                comp->halted = 1; // Stop on error
                exit(EXIT_FAILURE);
        }
    }
}

// --- Scaffold Analysis ---
char grid[MAX_GRID_DIM][MAX_GRID_DIM];
int grid_width = 0, grid_height = 0;

void parse_map(IntcodeComputer* comp) {
    int x = 0, y = 0;
    grid_width = 0;
    grid_height = 0;
    memset(grid, 0, sizeof(grid));

    while(comp->outputs.count > 0) {
        ll val = dequeue(&comp->outputs);
        if (val == 10) { // Newline
            if (x > 0) {
                if (grid_width == 0) grid_width = x;
                y++;
                x = 0;
                 if (y >= MAX_GRID_DIM) { fprintf(stderr,"Grid height exceeds limit\n"); exit(1);}
            }
        } else {
             if (x >= MAX_GRID_DIM) { fprintf(stderr,"Grid width exceeds limit\n"); exit(1);}
            grid[y][x++] = (char)val;
        }
    }
     grid_height = y; // Height is number of lines processed
     // Handle case where last line doesn't end with newline (though AoC usually does)
     if (x > 0 && grid_width == 0) grid_width = x;
     if (x > 0 && y == grid_height) grid_height++;


}

int is_scaffold(int r, int c) {
    return r >= 0 && r < grid_height && c >= 0 && c < grid_width && grid[r][c] == '#';
}

ll calculate_alignment_sum() {
    ll total_sum = 0;
    for (int r = 1; r < grid_height - 1; ++r) {
        for (int c = 1; c < grid_width - 1; ++c) {
            if (grid[r][c] == '#' &&
                grid[r - 1][c] == '#' && grid[r + 1][c] == '#' &&
                grid[r][c - 1] == '#' && grid[r][c + 1] == '#') {
                total_sum += (ll)r * c;
            }
        }
    }
    return total_sum;
}

// --- Pathfinding ---
typedef struct {
    int x, y;
    char dir;
} RobotState;

RobotState find_robot() {
    for (int r = 0; r < grid_height; ++r) {
        for (int c = 0; c < grid_width; ++c) {
            if (strchr("^v<>", grid[r][c])) {
                return (RobotState){c, r, grid[r][c]};
            }
        }
    }
    fprintf(stderr, "Robot not found\n");
    exit(EXIT_FAILURE);
}

char turn_left(char dir) {
    switch (dir) {
        case '^': return '<'; case '<': return 'v';
        case 'v': return '>'; case '>': return '^';
        default: return '?';
    }
}

char turn_right(char dir) {
    switch (dir) {
        case '^': return '>'; case '>': return 'v';
        case 'v': return '<'; case '<': return '^';
        default: return '?';
    }
}

void move_forward(int* x, int* y, char dir) {
    switch (dir) {
        case '^': (*y)--; break; case 'v': (*y)++; break;
        case '<': (*x)--; break; case '>': (*x)++; break;
    }
}

// --- Path Compression ---

char* path_tokens[MAX_TOKENS];
int path_token_count = 0;

// Dynamically allocate token strings
void add_path_token(const char* token) {
    if (path_token_count >= MAX_TOKENS) {
        fprintf(stderr, "Path token limit exceeded\n"); exit(1);
    }
    path_tokens[path_token_count] = strdup(token);
    if (!path_tokens[path_token_count]) { perror("strdup failed"); exit(1); }
    path_token_count++;
}

void free_path_tokens() {
     for (int i = 0; i < path_token_count; ++i) {
         free(path_tokens[i]);
     }
     path_token_count = 0; // Reset count after freeing
}

void get_movement_path(RobotState start) {
    int x = start.x;
    int y = start.y;
    char dir = start.dir;
    int steps = 0;
    free_path_tokens(); // Clear previous path if any

    while (1) {
        int next_x = x, next_y = y;
        move_forward(&next_x, &next_y, dir);

        if (is_scaffold(next_y, next_x)) {
            steps++;
            x = next_x;
            y = next_y;
        } else {
            if (steps > 0) {
                char step_str[10];
                sprintf(step_str, "%d", steps);
                add_path_token(step_str);
                steps = 0;
            }

            // Try turning left
            char left_dir = turn_left(dir);
            int left_x = x, left_y = y;
            move_forward(&left_x, &left_y, left_dir);
            if (is_scaffold(left_y, left_x)) {
                add_path_token("L");
                dir = left_dir;
                continue;
            }

            // Try turning right
            char right_dir = turn_right(dir);
            int right_x = x, right_y = y;
            move_forward(&right_x, &right_y, right_dir);
            if (is_scaffold(right_y, right_x)) {
                add_path_token("R");
                dir = right_dir;
                continue;
            }

            // No more moves
            break;
        }
    }
}


// Function to join tokens into a string
void join_tokens(char* dest, char** tokens, int start, int len, const char* sep) {
    dest[0] = '\0';
    for (int i = 0; i < len; ++i) {
        if (i > 0) strcat(dest, sep);
        strcat(dest, tokens[start + i]);
    }
}

// Check if a sequence of tokens matches a pattern
int tokens_match(char** tokens, int start, char** pattern, int pattern_len) {
     if (start + pattern_len > path_token_count) return 0; // Bounds check
    for (int i = 0; i < pattern_len; ++i) {
        if (strcmp(tokens[start + i], pattern[i]) != 0) {
            return 0;
        }
    }
    return 1;
}


int compress_path(char* main_routine, char* func_A, char* func_B, char* func_C) {
    char temp_str[MAX_FUNC_LEN];

    for (int a_len = 1; a_len <= 10 && a_len <= path_token_count; ++a_len) {
        join_tokens(func_A, path_tokens, 0, a_len, ",");
        if (strlen(func_A) >= MAX_FUNC_LEN) continue;

        int b_start = 0;
        while(b_start < path_token_count && tokens_match(path_tokens, b_start, path_tokens, a_len)) {
             b_start += a_len;
        }
         if (b_start >= path_token_count) continue; // Only found A

        for (int b_len = 1; b_len <= 10 && b_start + b_len <= path_token_count; ++b_len) {
            join_tokens(func_B, path_tokens, b_start, b_len, ",");
            if (strlen(func_B) >= MAX_FUNC_LEN) continue;

             int c_start = 0;
             while (c_start < path_token_count) {
                  if (tokens_match(path_tokens, c_start, path_tokens, a_len)) {
                       c_start += a_len;
                  } else if (tokens_match(path_tokens, c_start, path_tokens + b_start, b_len)) {
                       c_start += b_len;
                  } else {
                       break; // Found potential start for C
                  }
             }
              if (c_start >= path_token_count) continue; // Only found A and B


            for (int c_len = 1; c_len <= 10 && c_start + c_len <= path_token_count; ++c_len) {
                join_tokens(func_C, path_tokens, c_start, c_len, ",");
                if (strlen(func_C) >= MAX_FUNC_LEN) continue;

                // Try to build the main routine
                main_routine[0] = '\0';
                int current_pos = 0;
                int main_len = 0;
                while (current_pos < path_token_count) {
                     int found_match = 0;
                    if (tokens_match(path_tokens, current_pos, path_tokens, a_len)) {
                        if (main_len > 0) strcat(main_routine, ",");
                        strcat(main_routine, "A");
                        current_pos += a_len;
                        main_len++;
                        found_match = 1;
                    } else if (tokens_match(path_tokens, current_pos, path_tokens + b_start, b_len)) {
                         if (main_len > 0) strcat(main_routine, ",");
                        strcat(main_routine, "B");
                        current_pos += b_len;
                         main_len++;
                        found_match = 1;
                    } else if (tokens_match(path_tokens, current_pos, path_tokens + c_start, c_len)) {
                         if (main_len > 0) strcat(main_routine, ",");
                        strcat(main_routine, "C");
                        current_pos += c_len;
                        main_len++;
                        found_match = 1;
                    }

                    if (!found_match) break; // Cannot cover the path with A, B, C
                     if (strlen(main_routine) >= MAX_FUNC_LEN) break; // Main routine too long
                }

                if (current_pos == path_token_count && strlen(main_routine) < MAX_FUNC_LEN) {
                    return 1; // Found a valid compression
                }
            }
            func_C[0] = '\0'; // Reset C if inner loop failed
        }
         func_B[0] = '\0'; // Reset B if middle loop failed
    }
     func_A[0] = '\0'; // Reset A if outer loop failed
    return 0; // No compression found
}

int main() {
    FILE *infile = fopen("input.txt", "r");
    if (!infile) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    ll *program = NULL;
    size_t program_size = 0;
    size_t capacity = 100;
    program = malloc(capacity * sizeof(ll));
    if (!program) { perror("Malloc failed"); fclose(infile); return EXIT_FAILURE; }

    ll val;
    while (fscanf(infile, "%lld,", &val) == 1) {
        if (program_size >= capacity) {
            capacity *= 2;
            ll* temp = realloc(program, capacity * sizeof(ll));
            if (!temp) { perror("Realloc failed"); free(program); fclose(infile); return EXIT_FAILURE; }
            program = temp;
        }
        program[program_size++] = val;
    }
    fclose(infile);

    // --- Part One ---
    IntcodeComputer comp1;
    init_computer(&comp1, program, program_size);
    run_computer(&comp1);

    // Make sure outputs queue is large enough before creating array
    ll* output_array = malloc(comp1.outputs.count * sizeof(ll));
    if (!output_array) { perror("Malloc failed"); free_computer(&comp1); free(program); return EXIT_FAILURE; }
    queue_to_array(&comp1.outputs, output_array);
    size_t output_count = comp1.outputs.count;


    // Re-initialize outputs queue for parsing
    free_queue(&comp1.outputs);
    init_queue(&comp1.outputs, output_count);
     for(size_t i=0; i< output_count; ++i) {
        enqueue(&comp1.outputs, output_array[i]);
     }
    free(output_array);


    parse_map(&comp1);
    ll alignment_sum = calculate_alignment_sum();
    printf("Part One: %lld\n", alignment_sum);
    free_computer(&comp1);


    // --- Part Two ---
    program[0] = 2; // Modify program for part 2
    IntcodeComputer comp2;
    init_computer(&comp2, program, program_size);

    RobotState robot = find_robot();
    get_movement_path(robot);

    char main_routine[MAX_FUNC_LEN] = {0};
    char func_A[MAX_FUNC_LEN] = {0};
    char func_B[MAX_FUNC_LEN] = {0};
    char func_C[MAX_FUNC_LEN] = {0};

    if (!compress_path(main_routine, func_A, func_B, func_C)) {
        fprintf(stderr, "Failed to compress path\n");
         free_path_tokens();
         free_computer(&comp2);
         free(program);
        return EXIT_FAILURE;
    }

    char* inputs_str[] = {main_routine, func_A, func_B, func_C, "n"};
    for (int i = 0; i < 5; ++i) {
        for (char* p = inputs_str[i]; *p; ++p) {
            enqueue(&comp2.inputs, (ll)*p);
        }
        enqueue(&comp2.inputs, 10); // Newline
    }

    run_computer(&comp2); // Run until halt or blocked
    if(comp2.waiting_for_input && comp2.inputs.count > 0) {
         // Should not happen with 'n' input, but just in case
         run_computer(&comp2);
    }


    ll dust_collected = 0;
     while(comp2.outputs.count > 0) {
        dust_collected = dequeue(&comp2.outputs); // Last output is the answer
     }


    printf("Part Two: %lld\n", dust_collected);

    // --- Cleanup ---
    free_path_tokens();
    free_computer(&comp2);
    free(program);

    return EXIT_SUCCESS;
}
