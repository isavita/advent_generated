
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_MEMORY 10000
#define MAX_LINE_LENGTH 10000

long long intcode[MAX_MEMORY];
long long relative_base = 0;

long long get_param(int mode, long long pos) {
    switch (mode) {
        case 0: return intcode[intcode[pos]];
        case 1: return intcode[pos];
        case 2: return intcode[intcode[pos] + relative_base];
        default: printf("Invalid parameter mode\n"); exit(1);
    }
}

void set_param(int mode, long long pos, long long value) {
    switch (mode) {
        case 0: intcode[intcode[pos]] = value; break;
        case 2: intcode[intcode[pos] + relative_base] = value; break;
        default: printf("Invalid parameter mode for setting\n"); exit(1);
    }
}

long long run_intcode(long long input) {
    long long ip = 0;
    long long output = 0;
    
    while (intcode[ip] != 99) {
        long long opcode = intcode[ip] % 100;
        int mode1 = (intcode[ip] / 100) % 10;
        int mode2 = (intcode[ip] / 1000) % 10;
        int mode3 = (intcode[ip] / 10000) % 10;

        if (opcode == 1) {
            set_param(mode3, ip + 3, get_param(mode1, ip + 1) + get_param(mode2, ip + 2));
            ip += 4;
        } else if (opcode == 2) {
            set_param(mode3, ip + 3, get_param(mode1, ip + 1) * get_param(mode2, ip + 2));
            ip += 4;
        } else if (opcode == 3) {
            set_param(mode1, ip + 1, input);
            ip += 2;
        } else if (opcode == 4) {
            output = get_param(mode1, ip + 1);
            printf("%lld\n", output);
            ip += 2;
        } else if (opcode == 5) {
            if (get_param(mode1, ip + 1) != 0) {
                ip = get_param(mode2, ip + 2);
            } else {
                ip += 3;
            }
        } else if (opcode == 6) {
            if (get_param(mode1, ip + 1) == 0) {
                ip = get_param(mode2, ip + 2);
            } else {
                ip += 3;
            }
        } else if (opcode == 7) {
            set_param(mode3, ip + 3, (get_param(mode1, ip + 1) < get_param(mode2, ip + 2)) ? 1 : 0);
            ip += 4;
        } else if (opcode == 8) {
            set_param(mode3, ip + 3, (get_param(mode1, ip + 1) == get_param(mode2, ip + 2)) ? 1 : 0);
            ip += 4;
        } else if (opcode == 9) {
            relative_base += get_param(mode1, ip + 1);
            ip += 2;
        } else {
            printf("Invalid opcode: %lld\n", opcode);
            exit(1);
        }
    }
    return output;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    fgets(line, sizeof(line), file);
    fclose(file);

    memset(intcode, 0, sizeof(intcode));
    
    char *token = strtok(line, ",");
    int i = 0;
    while (token != NULL) {
        intcode[i++] = atoll(token);
        token = strtok(NULL, ",");
    }

    // Part 1
    relative_base = 0;
    run_intcode(1);

    // Part 2
    relative_base = 0;
    run_intcode(2);

    return 0;
}
