
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

typedef enum {
    Black = 0,
    White = 1
} PanelColor;

typedef enum {
    Up = 0,
    Right,
    Down,
    Left
} Direction;

typedef struct {
    int x, y;
} Position;

typedef struct {
    Position pos;
    Direction dir;
} Robot;

typedef struct {
    int key_x;
    int key_y;
    PanelColor value;
    struct Panel *next;
} Panel;

typedef struct {
    Panel *head;
} Grid;

void grid_init(Grid *grid) {
    grid->head = NULL;
}

Panel *grid_get(Grid *grid, int x, int y) {
    Panel *current = grid->head;
    while (current != NULL) {
        if (current->key_x == x && current->key_y == y) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

void grid_put(Grid *grid, int x, int y, PanelColor color) {
    Panel *existing = grid_get(grid, x, y);
    if (existing != NULL) {
        existing->value = color;
        return;
    }

    Panel *new_panel = malloc(sizeof(Panel));
    if (new_panel == NULL) {
        perror("Failed to allocate memory for new panel");
        exit(EXIT_FAILURE);
    }
    new_panel->key_x = x;
    new_panel->key_y = y;
    new_panel->value = color;
    new_panel->next = grid->head;
    grid->head = new_panel;
}

int grid_size(Grid *grid) {
  int count = 0;
  Panel *current = grid->head;
  while (current != NULL) {
    count++;
    current = current->next;
  }
  return count;
}

void robot_turn_and_move(Robot *robot, int turnDirection) {
    if (turnDirection == 0) {
        robot->dir = (robot->dir + 3) % 4;
    } else {
        robot->dir = (robot->dir + 1) % 4;
    }

    switch (robot->dir) {
        case Up:
            robot->pos.y--;
            break;
        case Right:
            robot->pos.x++;
            break;
        case Down:
            robot->pos.y++;
            break;
        case Left:
            robot->pos.x--;
            break;
    }
}

typedef struct {
    int *memory;
    size_t memory_size;
    int ip;
    int input[2];
    int input_len;
    int output[2];
    int output_len;
    bool halted;
} Intcode;

void intcode_init(Intcode *ic, int *program, size_t program_len) {
  ic->memory_size = program_len + 1000;
  ic->memory = malloc(sizeof(int) * ic->memory_size);
    if (ic->memory == NULL) {
        perror("Failed to allocate memory for intcode memory");
        exit(EXIT_FAILURE);
    }
  memcpy(ic->memory, program, sizeof(int) * program_len);
    ic->ip = 0;
    ic->input_len = 0;
    ic->output_len = 0;
    ic->halted = false;
}
void intcode_free(Intcode *ic){
  free(ic->memory);
}
void intcode_add_input(Intcode *ic, int input) {
  if (ic->input_len < 2) {
    ic->input[ic->input_len++] = input;
    }
}

void intcode_ensure_memory(Intcode *ic, size_t address) {
    if (address >= ic->memory_size) {
        size_t new_size = ic->memory_size * 2;
        if (address >= new_size) {
            new_size = address + 1000;
        }
        int *new_memory = realloc(ic->memory, sizeof(int) * new_size);
        if(new_memory == NULL){
          perror("Failed to reallocate memory");
          exit(EXIT_FAILURE);
        }
        ic->memory = new_memory;
        ic->memory_size = new_size;
    }
}
int intcode_read_memory(Intcode *ic, int address) {
    intcode_ensure_memory(ic, address);
    return ic->memory[address];
}

void intcode_write_memory(Intcode *ic, int address, int value) {
  intcode_ensure_memory(ic, address);
    ic->memory[address] = value;
}

int *intcode_get_params(Intcode *ic, int count) {
    int param_modes = ic->memory[ic->ip] / 100;
    static int params[3];
    for (int i = 0; i < count; i++) {
        if (param_modes % 10 == 1) {
            params[i] = ic->ip + i + 1;
        } else {
            params[i] = ic->memory[ic->ip + i + 1];
        }
        param_modes /= 10;
    }
    return params;
}

void intcode_run(Intcode *ic) {
    ic->output_len = 0;
    while (true) {
        int opcode = ic->memory[ic->ip] % 100;
        switch (opcode) {
            case 1:
            case 2:
            case 7:
            case 8: {
                int *params = intcode_get_params(ic, 3);
                int val1 = intcode_read_memory(ic, params[0]);
                int val2 = intcode_read_memory(ic, params[1]);
                int result;
                if (opcode == 1) {
                    result = val1 + val2;
                } else if (opcode == 2) {
                    result = val1 * val2;
                } else if ((opcode == 7 && val1 < val2) || (opcode == 8 && val1 == val2)) {
                    result = 1;
                } else {
                    result = 0;
                }
                intcode_write_memory(ic, params[2], result);
                ic->ip += 4;
                break;
            }
            case 3:
            case 4: {
                int *params = intcode_get_params(ic, 1);
                if (opcode == 3) {
                    if (ic->input_len == 0) {
                        return;
                    }
                    intcode_write_memory(ic, params[0], ic->input[0]);
                    ic->input_len--;
                    ic->input[0] = ic->input[1];
                } else {
                    ic->output[ic->output_len++] = intcode_read_memory(ic, params[0]);
                }
                ic->ip += 2;
                break;
            }
            case 5:
            case 6: {
                int *params = intcode_get_params(ic, 2);
                int val = intcode_read_memory(ic, params[0]);
                int target = intcode_read_memory(ic, params[1]);
                 if ((opcode == 5 && val != 0) || (opcode == 6 && val == 0)) {
                    ic->ip = target;
                } else {
                    ic->ip += 3;
                }
                break;
            }
            case 99:
                ic->halted = true;
                return;
            default:
                fprintf(stderr, "Unknown opcode: %d\n", opcode);
                exit(EXIT_FAILURE);
        }
    }
}

int main() {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    char *token;
    int program[10000];
    int program_len = 0;


    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Failed to open input file");
        return EXIT_FAILURE;
    }

    if ((read = getline(&line, &len, fp)) != -1) {
        token = strtok(line, ",");
        while (token != NULL) {
            program[program_len++] = atoi(token);
            token = strtok(NULL, ",");
        }
    }
    free(line);
    fclose(fp);

    Grid grid;
    grid_init(&grid);
    Robot robot = {{0, 0}, Up};
    Intcode intcode;
    intcode_init(&intcode, program, program_len);

    while (!intcode.halted) {
        Panel *panel = grid_get(&grid, robot.pos.x, robot.pos.y);
        int currentColor = panel == NULL ? Black : panel->value;
        intcode_add_input(&intcode, currentColor);
        intcode_run(&intcode);
        if (intcode.output_len == 2) {
            grid_put(&grid, robot.pos.x, robot.pos.y, intcode.output[0]);
            robot_turn_and_move(&robot, intcode.output[1]);
        }
    }

    printf("%d\n", grid_size(&grid));
    
    Panel *current = grid.head;
    while(current != NULL){
      Panel *next = current->next;
      free(current);
      current = next;
    }
    intcode_free(&intcode);

    return EXIT_SUCCESS;
}
