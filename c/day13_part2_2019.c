
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MEMORY_SIZE 10000
#define MAX_PROGRAM_SIZE 4096

typedef long long ll;

typedef struct {
    ll memory[MEMORY_SIZE];
    ll ip;
    ll relative_base;
    ll ball_x;
    ll paddle_x;
    ll score;
    int halted;
    ll temp_x;
    ll temp_y;
    int output_state; // 0: expecting x, 1: expecting y, 2: expecting tile_id
} IntcodeComputer;

ll get_addr(IntcodeComputer *comp, int offset, int mode) {
    ll param_addr = comp->ip + offset;
    if (param_addr < 0 || param_addr >= MEMORY_SIZE) {
         comp->halted = 1;
         return -1;
    }
    ll base_addr;
    switch (mode) {
        case 0: // Position mode
            base_addr = comp->memory[param_addr];
            break;
        case 1: // Immediate mode - Used for reading value directly
             return param_addr;
        case 2: // Relative mode
            base_addr = comp->relative_base + comp->memory[param_addr];
            break;
        default:
            comp->halted = 1;
            return -1;
    }
     if (base_addr < 0 || base_addr >= MEMORY_SIZE) {
         comp->halted = 1;
         return -1;
     }
    return base_addr;
}


ll get_param(IntcodeComputer *comp, int offset, int mode) {
    ll addr = get_addr(comp, offset, mode);
    if (comp->halted || addr == -1) return 0; // Return dummy value on error
    if (mode == 1) { // Immediate mode reads value directly
         return comp->memory[addr];
    }
     if (addr < 0 || addr >= MEMORY_SIZE) { // Check final address bounds
         comp->halted = 1;
         return 0;
     }
    return comp->memory[addr];
}

ll get_write_addr(IntcodeComputer *comp, int offset, int mode) {
    // Write address calculation is same as get_addr, but immediate mode is invalid
    if (mode == 1) {
        comp->halted = 1;
        return -1;
    }
    return get_addr(comp, offset, mode);
}


void run_game(IntcodeComputer *comp) {
    while (!comp->halted) {
        if (comp->ip < 0 || comp->ip >= MEMORY_SIZE) {
             comp->halted = 1;
             break;
        }
        ll instruction = comp->memory[comp->ip];
        int opcode = instruction % 100;
        int modes[3] = {(instruction / 100) % 10, (instruction / 1000) % 10, (instruction / 10000) % 10};
        ll addr, p1, p2;

        switch (opcode) {
            case 1: // Add
            case 2: // Multiply
                p1 = get_param(comp, 1, modes[0]);
                p2 = get_param(comp, 2, modes[1]);
                addr = get_write_addr(comp, 3, modes[2]);
                if (comp->halted || addr == -1) break;
                comp->memory[addr] = (opcode == 1) ? (p1 + p2) : (p1 * p2);
                comp->ip += 4;
                break;
            case 3: // Input
                addr = get_write_addr(comp, 1, modes[0]);
                if (comp->halted || addr == -1) break;
                ll input_val = 0;
                if (comp->ball_x > comp->paddle_x) input_val = 1;
                else if (comp->ball_x < comp->paddle_x) input_val = -1;
                comp->memory[addr] = input_val;
                comp->ip += 2;
                break;
            case 4: // Output
                {
                    ll output_val = get_param(comp, 1, modes[0]);
                    if (comp->halted) break;

                    if (comp->output_state == 0) {
                        comp->temp_x = output_val;
                        comp->output_state = 1;
                    } else if (comp->output_state == 1) {
                        comp->temp_y = output_val;
                        comp->output_state = 2;
                    } else {
                        ll tile_id = output_val;
                        if (comp->temp_x == -1 && comp->temp_y == 0) {
                            comp->score = tile_id;
                        } else {
                            if (tile_id == 3) { // Paddle
                                comp->paddle_x = comp->temp_x;
                            } else if (tile_id == 4) { // Ball
                                comp->ball_x = comp->temp_x;
                            }
                        }
                        comp->output_state = 0;
                    }
                    comp->ip += 2;
                }
                break;
            case 5: // Jump-if-true
            case 6: // Jump-if-false
                p1 = get_param(comp, 1, modes[0]);
                p2 = get_param(comp, 2, modes[1]);
                if (comp->halted) break;
                if ((opcode == 5 && p1 != 0) || (opcode == 6 && p1 == 0)) {
                    if (p2 < 0 || p2 >= MEMORY_SIZE) { comp->halted = 1; break; }
                    comp->ip = p2;
                } else {
                    comp->ip += 3;
                }
                break;
            case 7: // Less than
            case 8: // Equals
                p1 = get_param(comp, 1, modes[0]);
                p2 = get_param(comp, 2, modes[1]);
                addr = get_write_addr(comp, 3, modes[2]);
                if (comp->halted || addr == -1) break;
                comp->memory[addr] = ((opcode == 7 && p1 < p2) || (opcode == 8 && p1 == p2)) ? 1 : 0;
                comp->ip += 4;
                break;
            case 9: // Adjust relative base
                p1 = get_param(comp, 1, modes[0]);
                 if (comp->halted) break;
                comp->relative_base += p1;
                comp->ip += 2;
                break;
            case 99: // Halt
                comp->halted = 1;
                break;
            default:
                comp->halted = 1;
                break;
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        return 1;
    }

    ll program[MAX_PROGRAM_SIZE];
    int program_size = 0;
    ll num;
    while (fscanf(file, "%lld,", &num) == 1 && program_size < MAX_PROGRAM_SIZE) {
        program[program_size++] = num;
    }
    fclose(file);

    if (program_size == 0) {
         return 1;
    }

    IntcodeComputer comp = {0};
    memcpy(comp.memory, program, program_size * sizeof(ll));

    comp.memory[0] = 2; // Free play mode

    run_game(&comp);

    printf("%lld\n", comp.score);

    return 0;
}

