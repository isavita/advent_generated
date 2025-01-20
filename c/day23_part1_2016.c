
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_INSTRUCTIONS 1000
#define MAX_LINE_LENGTH 50

typedef struct {
    char op[4];
    char arg1[4];
    char arg2[4];
} Instruction;

int registers[26];
Instruction instructions[MAX_INSTRUCTIONS];
int numInstructions = 0;

int getValue(char *s) {
    if (isdigit(s[0]) || (s[0] == '-' && isdigit(s[1])))
        return atoi(s);
    return registers[s[0] - 'a'];
}

void toggleInstruction(Instruction *instr) {
    if (strcmp(instr->op, "inc") == 0) {
        strcpy(instr->op, "dec");
    } else if (strcmp(instr->op, "dec") == 0 || strcmp(instr->op, "tgl") == 0) {
        strcpy(instr->op, "inc");
    } else if (strcmp(instr->op, "jnz") == 0) {
        strcpy(instr->op, "cpy");
    } else if (strcmp(instr->op, "cpy") == 0) {
        strcpy(instr->op, "jnz");
    }
}

void executeInstructions() {
    int pc = 0;
    while (pc < numInstructions) {
        Instruction *instr = &instructions[pc];
        if (strcmp(instr->op, "cpy") == 0) {
           if (isalpha(instr->arg2[0])) {
                registers[instr->arg2[0] - 'a'] = getValue(instr->arg1);
           }
        } else if (strcmp(instr->op, "inc") == 0) {
             if (isalpha(instr->arg1[0]))
                registers[instr->arg1[0] - 'a']++;
        } else if (strcmp(instr->op, "dec") == 0) {
            if (isalpha(instr->arg1[0]))
                registers[instr->arg1[0] - 'a']--;
        } else if (strcmp(instr->op, "jnz") == 0) {
            if (getValue(instr->arg1) != 0) {
                pc += getValue(instr->arg2) - 1;
            }
        } else if (strcmp(instr->op, "tgl") == 0) {
            int tgt = pc + getValue(instr->arg1);
            if (tgt >= 0 && tgt < numInstructions) {
                toggleInstruction(&instructions[tgt]);
            }
        }
        pc++;
    }
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), fp) != NULL) {
        sscanf(line, "%s %s %s", instructions[numInstructions].op, instructions[numInstructions].arg1, instructions[numInstructions].arg2);
        numInstructions++;
    }
    fclose(fp);

    registers['a' - 'a'] = 7;
    executeInstructions();
    printf("%d\n", registers['a' - 'a']);
    return 0;
}
