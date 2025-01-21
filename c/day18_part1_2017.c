
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 1000
#define MAX_LINE_LENGTH 50

typedef struct {
    char op[4];
    char arg1[20];
    char arg2[20];
} Instruction;

long long get_value(long long registers[], char *arg) {
    if (arg[0] >= 'a' && arg[0] <= 'z') {
        return registers[arg[0] - 'a'];
    } else {
        return atoll(arg);
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Instruction instructions[MAX_LINES];
    int num_instructions = 0;
    char line[MAX_LINE_LENGTH];

    while (fgets(line, sizeof(line), file) != NULL) {
        sscanf(line, "%s %s %s", instructions[num_instructions].op, 
               instructions[num_instructions].arg1, instructions[num_instructions].arg2);
        num_instructions++;
    }
    fclose(file);

    long long registers[26] = {0};
    long long last_sound = 0;
    long long recovered_frequency = 0;
    int ip = 0;

    while (ip >= 0 && ip < num_instructions) {
        Instruction *instr = &instructions[ip];

        if (strcmp(instr->op, "snd") == 0) {
            last_sound = get_value(registers, instr->arg1);
        } else if (strcmp(instr->op, "set") == 0) {
            registers[instr->arg1[0] - 'a'] = get_value(registers, instr->arg2);
        } else if (strcmp(instr->op, "add") == 0) {
            registers[instr->arg1[0] - 'a'] += get_value(registers, instr->arg2);
        } else if (strcmp(instr->op, "mul") == 0) {
            registers[instr->arg1[0] - 'a'] *= get_value(registers, instr->arg2);
        } else if (strcmp(instr->op, "mod") == 0) {
            registers[instr->arg1[0] - 'a'] %= get_value(registers, instr->arg2);
        } else if (strcmp(instr->op, "rcv") == 0) {
            if (get_value(registers, instr->arg1) != 0) {
                recovered_frequency = last_sound;
                break;
            }
        } else if (strcmp(instr->op, "jgz") == 0) {
            if (get_value(registers, instr->arg1) > 0) {
                ip += get_value(registers, instr->arg2) - 1;
            }
        }
        ip++;
    }

    printf("%lld\n", recovered_frequency);

    return 0;
}
