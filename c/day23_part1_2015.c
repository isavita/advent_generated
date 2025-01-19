
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_INSTRUCTIONS 100

typedef struct {
    char instruction[4];
    char reg;
    int offset;
} Instruction;

int main() {
    FILE *fp;
    char line[100];
    Instruction instructions[MAX_INSTRUCTIONS];
    int instructionCount = 0;
    int a = 0, b = 0;
    int pc = 0;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *token;
        char tempLine[100];
        strcpy(tempLine, line);
        token = strtok(tempLine, " ,");
        strcpy(instructions[instructionCount].instruction, token);

        if (strcmp(token, "hlf") == 0 || strcmp(token, "tpl") == 0 || strcmp(token, "inc") == 0) {
            token = strtok(NULL, " ,");
            instructions[instructionCount].reg = token[0];
            instructions[instructionCount].offset = 0;
        } else if (strcmp(token, "jmp") == 0) {
            token = strtok(NULL, " ,");
            instructions[instructionCount].reg = ' '; 
             instructions[instructionCount].offset = atoi(token);
        } else if (strcmp(token, "jie") == 0 || strcmp(token, "jio") == 0){
             token = strtok(NULL, " ,");
            instructions[instructionCount].reg = token[0];
            token = strtok(NULL, " ,");
             instructions[instructionCount].offset = atoi(token);
            
        }

        instructionCount++;
    }

    fclose(fp);

    while (pc >= 0 && pc < instructionCount) {
        Instruction current = instructions[pc];
        if (strcmp(current.instruction, "hlf") == 0) {
             if (current.reg == 'a')
                 a /= 2;
            else
                b /= 2;
            pc++;
        } else if (strcmp(current.instruction, "tpl") == 0) {
            if (current.reg == 'a')
                 a *= 3;
            else
                b *= 3;
            pc++;
        } else if (strcmp(current.instruction, "inc") == 0) {
             if (current.reg == 'a')
                 a++;
            else
                b++;
            pc++;
        } else if (strcmp(current.instruction, "jmp") == 0) {
            pc += current.offset;
        } else if (strcmp(current.instruction, "jie") == 0) {
             int regVal = (current.reg == 'a' ? a : b);
             if (regVal % 2 == 0)
                pc += current.offset;
            else
                pc++;

        } else if (strcmp(current.instruction, "jio") == 0) {
           int regVal = (current.reg == 'a' ? a : b);
             if (regVal == 1)
                pc += current.offset;
             else
                pc++;
        }
        
    }

    printf("%d\n", b);

    return 0;
}
