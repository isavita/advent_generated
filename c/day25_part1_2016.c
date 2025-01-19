
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define MAX_INSTRUCTIONS 1000
#define MAX_LINE_LENGTH 50

typedef struct {
    char instruction[4];
    char arg1[4];
    char arg2[4];
} Instruction;

int registers[4];
Instruction instructions[MAX_INSTRUCTIONS];
int numInstructions = 0;

int getValue(char *s) {
    if (isdigit(s[0]) || (s[0] == '-' && isdigit(s[1]))) {
        return atoi(s);
    }
    return registers[s[0] - 'a'];
}

bool producesClockSignal() {
    int lastOutput = -1;
    int outputCount = 0;
    int i = 0;

    while (i < numInstructions) {
        Instruction *instr = &instructions[i];
        if (strcmp(instr->instruction, "cpy") == 0) {
            registers[instr->arg2[0] - 'a'] = getValue(instr->arg1);
        } else if (strcmp(instr->instruction, "inc") == 0) {
            registers[instr->arg1[0] - 'a']++;
        } else if (strcmp(instr->instruction, "dec") == 0) {
            registers[instr->arg1[0] - 'a']--;
        } else if (strcmp(instr->instruction, "jnz") == 0) {
            if (getValue(instr->arg1) != 0) {
                i += getValue(instr->arg2);
                continue;
            }
        } else if (strcmp(instr->instruction, "out") == 0) {
            int val = getValue(instr->arg1);
             if (val != 0 && val != 1) {
                return false;
            }

            if (outputCount > 0 && val == lastOutput) {
                return false;
            }
             lastOutput = val;
            outputCount++;
             if (outputCount > 50) {
                return true;
            }
        }
        i++;
    }
    return false;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        char *token;
        token = strtok(line, " \n");
        if (token) {
             strcpy(instructions[numInstructions].instruction, token);

            token = strtok(NULL, " \n");
            if(token)
                strcpy(instructions[numInstructions].arg1, token);

            token = strtok(NULL, " \n");
            if(token)
                strcpy(instructions[numInstructions].arg2, token);
            numInstructions++;
        }
    }
    fclose(file);
    
    for (int a = 1; ; a++) {
        registers[0] = a;
        registers[1] = 0;
        registers[2] = 0;
        registers[3] = 0;
        if (producesClockSignal()) {
            printf("%d\n", a);
            break;
        }
    }
    return 0;
}
