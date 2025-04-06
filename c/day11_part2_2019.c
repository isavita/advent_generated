
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#define MEMORY_SIZE 8192
#define PANEL_MAP_SIZE 200 // Assuming robot stays within -100 to +99 grid

typedef struct {
    int64_t memory[MEMORY_SIZE];
    int64_t ip;
    int64_t relative_base;
    bool halted;
    int64_t input_value;
    bool awaiting_input;
} IntcodeComputer;

typedef struct {
    int x;
    int y;
} Point;

typedef struct {
    IntcodeComputer computer;
    Point position;
    int direction; // 0: Up, 1: Right, 2: Down, 3: Left
    int panels[PANEL_MAP_SIZE][PANEL_MAP_SIZE]; // 0: Black, 1: White, -1: Unpainted
    bool painted[PANEL_MAP_SIZE][PANEL_MAP_SIZE];
    int painted_count;
} Robot;

int64_t get_mem(IntcodeComputer* comp, int64_t address) {
    if (address < 0 || address >= MEMORY_SIZE) {
        fprintf(stderr, "Error: Memory access out of bounds: %lld\n", address);
        exit(EXIT_FAILURE);
    }
    return comp->memory[address];
}

void set_mem(IntcodeComputer* comp, int64_t address, int64_t value) {
    if (address < 0 || address >= MEMORY_SIZE) {
        fprintf(stderr, "Error: Memory write out of bounds: %lld\n", address);
        exit(EXIT_FAILURE);
    }
    comp->memory[address] = value;
}

int64_t get_param_address(IntcodeComputer* comp, int mode, int offset) {
    int64_t param = get_mem(comp, comp->ip + offset);
    switch (mode) {
        case 0: return param; // Position mode
        case 1: return comp->ip + offset; // Immediate mode (for reading value)
        case 2: return comp->relative_base + param; // Relative mode
        default:
            fprintf(stderr, "Error: Unknown parameter mode: %d\n", mode);
            exit(EXIT_FAILURE);
    }
}

int64_t get_param_value(IntcodeComputer* comp, int mode, int offset) {
    if (mode == 1) { // Immediate mode
        return get_mem(comp, comp->ip + offset);
    }
    return get_mem(comp, get_param_address(comp, mode, offset));
}

typedef enum {
    RUN_STATE_HALTED,
    RUN_STATE_AWAITING_INPUT,
    RUN_STATE_HAS_OUTPUT
} RunState;

RunState run_computer(IntcodeComputer* comp, int64_t* output) {
    comp->awaiting_input = false;
    while (true) {
        int64_t instruction = get_mem(comp, comp->ip);
        int opcode = instruction % 100;
        int modes[3] = {
            (int)((instruction / 100) % 10),
            (int)((instruction / 1000) % 10),
            (int)((instruction / 10000) % 10)
        };

        switch (opcode) {
            case 1: { // Addition
                int64_t val1 = get_param_value(comp, modes[0], 1);
                int64_t val2 = get_param_value(comp, modes[1], 2);
                int64_t dest_addr = get_param_address(comp, modes[2], 3);
                set_mem(comp, dest_addr, val1 + val2);
                comp->ip += 4;
                break;
            }
            case 2: { // Multiplication
                int64_t val1 = get_param_value(comp, modes[0], 1);
                int64_t val2 = get_param_value(comp, modes[1], 2);
                int64_t dest_addr = get_param_address(comp, modes[2], 3);
                set_mem(comp, dest_addr, val1 * val2);
                comp->ip += 4;
                break;
            }
            case 3: { // Input
                comp->awaiting_input = true;
                return RUN_STATE_AWAITING_INPUT; // Pause to get input
            }
             case 33: { // Resume Input after providing value
                int64_t dest_addr = get_param_address(comp, modes[0], 1);
                set_mem(comp, dest_addr, comp->input_value);
                comp->ip += 2;
                 comp->awaiting_input = false;
                break;
            }
            case 4: { // Output
                *output = get_param_value(comp, modes[0], 1);
                comp->ip += 2;
                return RUN_STATE_HAS_OUTPUT; // Return with output
            }
            case 5: { // Jump-if-true
                int64_t val1 = get_param_value(comp, modes[0], 1);
                int64_t val2 = get_param_value(comp, modes[1], 2);
                if (val1 != 0) {
                    comp->ip = val2;
                } else {
                    comp->ip += 3;
                }
                break;
            }
            case 6: { // Jump-if-false
                int64_t val1 = get_param_value(comp, modes[0], 1);
                int64_t val2 = get_param_value(comp, modes[1], 2);
                if (val1 == 0) {
                    comp->ip = val2;
                } else {
                    comp->ip += 3;
                }
                break;
            }
            case 7: { // Less than
                int64_t val1 = get_param_value(comp, modes[0], 1);
                int64_t val2 = get_param_value(comp, modes[1], 2);
                int64_t dest_addr = get_param_address(comp, modes[2], 3);
                set_mem(comp, dest_addr, (val1 < val2) ? 1 : 0);
                comp->ip += 4;
                break;
            }
            case 8: { // Equals
                int64_t val1 = get_param_value(comp, modes[0], 1);
                int64_t val2 = get_param_value(comp, modes[1], 2);
                int64_t dest_addr = get_param_address(comp, modes[2], 3);
                set_mem(comp, dest_addr, (val1 == val2) ? 1 : 0);
                comp->ip += 4;
                break;
            }
            case 9: { // Adjust relative base
                int64_t val1 = get_param_value(comp, modes[0], 1);
                comp->relative_base += val1;
                comp->ip += 2;
                break;
            }
            case 99: { // Halt
                comp->halted = true;
                return RUN_STATE_HALTED;
            }
            default:
                fprintf(stderr, "Error: Unknown opcode %d at ip %lld\n", opcode, comp->ip);
                exit(EXIT_FAILURE);
        }
    }
}


void init_robot(Robot* robot, const int64_t* program, int start_panel_color) {
    memset(robot->computer.memory, 0, sizeof(robot->computer.memory));
    memcpy(robot->computer.memory, program, MEMORY_SIZE * sizeof(int64_t)); // Assuming program fits initially
    robot->computer.ip = 0;
    robot->computer.relative_base = 0;
    robot->computer.halted = false;
    robot->computer.awaiting_input = false;

    robot->position.x = PANEL_MAP_SIZE / 2;
    robot->position.y = PANEL_MAP_SIZE / 2;
    robot->direction = 0; // Up

    for (int y = 0; y < PANEL_MAP_SIZE; ++y) {
        for (int x = 0; x < PANEL_MAP_SIZE; ++x) {
            robot->panels[y][x] = -1; // Unpainted
            robot->painted[y][x] = false;
        }
    }
    robot->panels[robot->position.y][robot->position.x] = start_panel_color;
    robot->painted_count = 0;
}

void turn_and_move(Robot* robot, int turn_direction) {
    if (turn_direction == 0) { // Turn left
        robot->direction = (robot->direction + 3) % 4; // (dir - 1 + 4) % 4
    } else { // Turn right (turn_direction == 1)
        robot->direction = (robot->direction + 1) % 4;
    }

    switch (robot->direction) {
        case 0: robot->position.y--; break; // Up
        case 1: robot->position.x++; break; // Right
        case 2: robot->position.y++; break; // Down
        case 3: robot->position.x--; break; // Left
    }

    if (robot->position.x < 0 || robot->position.x >= PANEL_MAP_SIZE ||
        robot->position.y < 0 || robot->position.y >= PANEL_MAP_SIZE) {
        fprintf(stderr, "Error: Robot moved out of bounds (%d, %d)\n", robot->position.x, robot->position.y);
        exit(EXIT_FAILURE);
    }
}

void run_robot(Robot* robot) {
    RunState state;
    int64_t output_value;
    bool expecting_paint_color = true; // True if next output is color, False if turn

    while (!robot->computer.halted) {
        state = run_computer(&robot->computer, &output_value);

        switch (state) {
            case RUN_STATE_HALTED:
                break;

            case RUN_STATE_AWAITING_INPUT:
                 {
                    int current_color = robot->panels[robot->position.y][robot->position.x];
                    if (current_color == -1) current_color = 0; // Unpainted is black
                    robot->computer.input_value = current_color;
                    // Modify opcode to resume input processing
                     if (robot->computer.memory[robot->computer.ip] % 100 == 3) {
                        robot->computer.memory[robot->computer.ip] = (robot->computer.memory[robot->computer.ip] / 100) * 100 + 33;
                     } else {
                         fprintf(stderr, "Error: Awaiting input but opcode is not 3\n");
                         exit(EXIT_FAILURE);
                     }

                }
                break;

            case RUN_STATE_HAS_OUTPUT:
                if (expecting_paint_color) {
                    // Paint the current panel
                    int paint_color = (int)output_value;
                    if (!robot->painted[robot->position.y][robot->position.x]) {
                        robot->painted[robot->position.y][robot->position.x] = true;
                        robot->painted_count++;
                    }
                    robot->panels[robot->position.y][robot->position.x] = paint_color;
                    expecting_paint_color = false;
                } else {
                    // Turn and move
                    int turn_direction = (int)output_value;
                    turn_and_move(robot, turn_direction);
                    expecting_paint_color = true;
                }
                break;
        }
    }
}

void render_panels(Robot* robot) {
    int min_x = PANEL_MAP_SIZE, max_x = -1, min_y = PANEL_MAP_SIZE, max_y = -1;

    for (int y = 0; y < PANEL_MAP_SIZE; ++y) {
        for (int x = 0; x < PANEL_MAP_SIZE; ++x) {
            if (robot->panels[y][x] != -1) {
                if (x < min_x) min_x = x;
                if (x > max_x) max_x = x;
                if (y < min_y) min_y = y;
                if (y > max_y) max_y = y;
            }
        }
    }

    if (max_x < min_x) {
        printf("No panels painted.\n");
        return;
    }

    printf("\nRegistration Identifier:\n");
    for (int y = min_y; y <= max_y; ++y) {
        for (int x = min_x; x <= max_x; ++x) {
            int color = robot->panels[y][x];
             if (color == -1) color = 0; // Treat unpainted as black for rendering
            putchar((color == 1) ? '#' : ' ');
        }
        putchar('\n');
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    static int64_t initial_program[MEMORY_SIZE] = {0};
    int program_size = 0;
    char buffer[16 * MEMORY_SIZE]; // Adjust size as needed
    if (fgets(buffer, sizeof(buffer), file) == NULL) {
         fprintf(stderr, "Error reading input file or file empty.\n");
         fclose(file);
         return EXIT_FAILURE;
    }
    fclose(file);

    char *token = strtok(buffer, ",");
    while (token != NULL && program_size < MEMORY_SIZE) {
        initial_program[program_size++] = strtoll(token, NULL, 10);
        token = strtok(NULL, ",");
    }
     if (program_size == 0) {
         fprintf(stderr, "No program data found in input file.\n");
         return EXIT_FAILURE;
     }


    // Part One
    Robot robot1;
    init_robot(&robot1, initial_program, 0); // Start on black panel
    run_robot(&robot1);
    printf("Part One: %d\n", robot1.painted_count);

    // Part Two
    Robot robot2;
    init_robot(&robot2, initial_program, 1); // Start on white panel
    run_robot(&robot2);
    printf("Part Two:");
    render_panels(&robot2);

    return EXIT_SUCCESS;
}
