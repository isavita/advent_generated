
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef long long ll;

typedef enum {
    POSITION = 0,
    IMMEDIATE,
    RELATIVE
} mode;

typedef enum {
    ADD = 1,
    MUL,
    INPUT,
    OUTPUT,
    JT,
    JF,
    LT,
    EQ,
    RBO,
    HALT = 99
} opcode;

typedef struct {
    ll x, y;
} Point;

typedef enum {
    N = 0,
    E,
    S,
    W
} Dir;

Point points[] = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}};

ll get_value(ll *data, ll *size, ll i, mode mo, ll relbase) {
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

void set_value(ll *data, ll *size, ll i, mode mo, ll val, ll relbase) {
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

int run_program(ll *program, ll psize) {
    ll data[10000];
    memcpy(data, program, sizeof(ll) * psize);
    ll size = 10000;

    ll ip = 0;
    ll relbase = 0;
    
    char output_buffer[10000];
    int buffer_index = 0;

    while (1) {
        ll op_full = data[ip];
        opcode op = op_full % 100;
        mode modes[3] = {
            (op_full / 100) % 10,
            (op_full / 1000) % 10,
            (op_full / 10000) % 10
        };

        if (op == ADD) {
            ll val = get_value(data, &size, ip + 1, modes[0], relbase) + get_value(data, &size, ip + 2, modes[1], relbase);
            set_value(data, &size, ip + 3, modes[2], val, relbase);
            ip += 4;
        } else if (op == MUL) {
            ll val = get_value(data, &size, ip + 1, modes[0], relbase) * get_value(data, &size, ip + 2, modes[1], relbase);
            set_value(data, &size, ip + 3, modes[2], val, relbase);
            ip += 4;
        } else if (op == INPUT) {
            set_value(data, &size, ip + 1, modes[0], 0, relbase);
            ip += 2;
        } else if (op == OUTPUT) {
            output_buffer[buffer_index++] = (char)get_value(data, &size, ip + 1, modes[0], relbase);
            ip += 2;
        } else if (op == JT) {
            if (get_value(data, &size, ip + 1, modes[0], relbase) != 0) {
                ip = get_value(data, &size, ip + 2, modes[1], relbase);
            } else {
                ip += 3;
            }
        } else if (op == JF) {
            if (get_value(data, &size, ip + 1, modes[0], relbase) == 0) {
                ip = get_value(data, &size, ip + 2, modes[1], relbase);
            } else {
                ip += 3;
            }
        } else if (op == LT) {
            if (get_value(data, &size, ip + 1, modes[0], relbase) < get_value(data, &size, ip + 2, modes[1], relbase)) {
                set_value(data, &size, ip + 3, modes[2], 1, relbase);
            } else {
                set_value(data, &size, ip + 3, modes[2], 0, relbase);
            }
            ip += 4;
        } else if (op == EQ) {
            if (get_value(data, &size, ip + 1, modes[0], relbase) == get_value(data, &size, ip + 2, modes[1], relbase)) {
                set_value(data, &size, ip + 3, modes[2], 1, relbase);
            } else {
                set_value(data, &size, ip + 3, modes[2], 0, relbase);
            }
            ip += 4;
        } else if (op == RBO) {
            relbase += get_value(data, &size, ip + 1, modes[0], relbase);
            ip += 2;
        } else if (op == HALT) {
            break;
        }
    }
    output_buffer[buffer_index] = 0;

    int grid[100][100] = {0};
    int x = 0, y = 0;
    for(int i = 0; output_buffer[i]; ++i){
        if(output_buffer[i] == '\n'){
            x = 0;
            y++;
        } else {
            if(output_buffer[i] == '#' || output_buffer[i] == '^' || output_buffer[i] == 'v' || output_buffer[i] == '<' || output_buffer[i] == '>'){
                grid[y][x] = 1;
            }
            x++;
        }
    }

    int sum = 0;
    for(int i = 1; i < 99; ++i){
        for(int j = 1; j < 99; ++j){
            if(grid[i][j] && grid[i-1][j] && grid[i+1][j] && grid[i][j-1] && grid[i][j+1]){
                sum += i * j;
            }
        }
    }
    return sum;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    char line[100000];
    fgets(line, sizeof(line), file);
    fclose(file);

    ll program[1000];
    int psize = 0;
    char *token = strtok(line, ",");
    while (token != NULL) {
        program[psize++] = atoll(token);
        token = strtok(NULL, ",");
    }

    printf("%d\n", run_program(program, psize));

    return 0;
}
