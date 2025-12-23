
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef long long ll;

typedef enum {
    POSITION = 0,
    IMMEDIATE,
    RELATIVE
} Mode;

typedef enum {
    ADD = 1,
    MUL = 2,
    INPUT = 3,
    OUTPUT = 4,
    JT = 5,
    JF = 6,
    LT = 7,
    EQ = 8,
    RBO = 9,
    HALT = 99
} Opcode;

ll get_value(ll *data, ll *size, ll i, Mode mo, ll relbase) {
    if (i >= *size) {
        *size = i + 1;
    }
    if (mo == IMMEDIATE) {
        return data[i];
    } else if (mo == POSITION) {
        if (data[i] >= *size) {
            *size = data[i] + 1;
        }
        return data[data[i]];
    } else {
        if (relbase + data[i] >= *size) {
            *size = relbase + data[i] + 1;
        }
        return data[relbase + data[i]];
    }
}

void set_value(ll *data, ll *size, ll i, Mode mo, ll val, ll relbase) {
    if (mo == POSITION) {
        if (data[i] >= *size) {
            *size = data[i] + 1;
        }
        data[data[i]] = val;
    } else {
        if (relbase + data[i] >= *size) {
            *size = relbase + data[i] + 1;
        }
        data[relbase + data[i]] = val;
    }
}

ll run_program(ll *program, ll psize) {
    ll capacity = 10000;
    ll *data = malloc(capacity * sizeof(ll));
    memcpy(data, program, sizeof(ll) * psize);
    ll size = capacity;

    ll ip = 0;
    ll relbase = 0;

    char output_buffer[10000];
    int buffer_index = 0;

    while (1) {
        ll op_full = data[ip];
        Opcode op = op_full % 100;
        Mode modes[3] = {
            (op_full / 100) % 10,
            (op_full / 1000) % 10,
            (op_full / 10000) % 10
        };

        if (op == ADD) {
            ll a = get_value(data, &size, ip + 1, modes[0], relbase);
            ll b = get_value(data, &size, ip + 2, modes[1], relbase);
            ll val = a + b;
            set_value(data, &size, ip + 3, modes[2], val, relbase);
            ip += 4;
        } else if (op == MUL) {
            ll a = get_value(data, &size, ip + 1, modes[0], relbase);
            ll b = get_value(data, &size, ip + 2, modes[1], relbase);
            ll val = a * b;
            set_value(data, &size, ip + 3, modes[2], val, relbase);
            ip += 4;
        } else if (op == INPUT) {
            set_value(data, &size, ip + 1, modes[0], 0, relbase);
            ip += 2;
        } else if (op == OUTPUT) {
            ll val = get_value(data, &size, ip + 1, modes[0], relbase);
            output_buffer[buffer_index++] = (char)val;
            ip += 2;
        } else if (op == JT) {
            ll cond = get_value(data, &size, ip + 1, modes[0], relbase);
            ll dest = get_value(data, &size, ip + 2, modes[1], relbase);
            if (cond != 0) {
                ip = dest;
            } else {
                ip += 3;
            }
        } else if (op == JF) {
            ll cond = get_value(data, &size, ip + 1, modes[0], relbase);
            ll dest = get_value(data, &size, ip + 2, modes[1], relbase);
            if (cond == 0) {
                ip = dest;
            } else {
                ip += 3;
            }
        } else if (op == LT) {
            ll a = get_value(data, &size, ip + 1, modes[0], relbase);
            ll b = get_value(data, &size, ip + 2, modes[1], relbase);
            ll val = (a < b) ? 1 : 0;
            set_value(data, &size, ip + 3, modes[2], val, relbase);
            ip += 4;
        } else if (op == EQ) {
            ll a = get_value(data, &size, ip + 1, modes[0], relbase);
            ll b = get_value(data, &size, ip + 2, modes[1], relbase);
            ll val = (a == b) ? 1 : 0;
            set_value(data, &size, ip + 3, modes[2], val, relbase);
            ip += 4;
        } else if (op == RBO) {
            relbase += get_value(data, &size, ip + 1, modes[0], relbase);
            ip += 2;
        } else if (op == HALT) {
            break;
        }
    }
    output_buffer[buffer_index] = '\0';

    int grid[100][100] = {0};
    int grid_width = 0;
    int grid_height = 0;
    int x = 0, y = 0;
    for (int i = 0; output_buffer[i]; i++) {
        if (output_buffer[i] == '\n') {
            grid_width = (x > grid_width) ? x : grid_width;
            x = 0;
            y++;
        } else {
            if (output_buffer[i] == '#' || output_buffer[i] == '^' || output_buffer[i] == 'v' || 
                output_buffer[i] == '<' || output_buffer[i] == '>') {
                grid[y][x] = 1;
            }
            x++;
        }
    }
    grid_height = y;

    ll sum = 0;
    for (int i = 1; i < grid_height; i++) {
        for (int j = 1; j < grid_width; j++) {
            if (grid[i][j] && grid[i-1][j] && grid[i+1][j] && grid[i][j-1] && grid[i][j+1]) {
                sum += i * j;
            }
        }
    }

    free(data);
    return sum;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (!file) {
            printf("Failed to open the file.\n");
            return 1;
        }
        char line[100000];
        if (fgets(line, sizeof(line), file) == NULL) {
            printf("Error reading file.\n");
            fclose(file);
            return 1;
        }
        fclose(file);

        ll program[1000];
        int psize = 0;
        char *token = strtok(line, ",");
        while (token != NULL) {
            program[psize++] = atoll(token);
            token = strtok(NULL, ",");
        }

        ll result = run_program(program, psize);
        printf("%lld\n", result);
    }
    return 0;
}
