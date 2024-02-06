
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_REG 1000
#define LINE_LENGTH 100

typedef struct {
    char name[10];
    int value;
} Register;

Register registers[MAX_REG];
int regCount = 0;

int getRegisterIndex(char* name) {
    for (int i = 0; i < regCount; i++) {
        if (strcmp(registers[i].name, name) == 0) {
            return i;
        }
    }
    strcpy(registers[regCount].name, name);
    registers[regCount].value = 0;
    return regCount++;
}

int checkCondition(int regIndex, char* op, int value) {
    if (strcmp(op, ">") == 0) return registers[regIndex].value > value;
    if (strcmp(op, ">=") == 0) return registers[regIndex].value >= value;
    if (strcmp(op, "<") == 0) return registers[regIndex].value < value;
    if (strcmp(op, "<=") == 0) return registers[regIndex].value <= value;
    if (strcmp(op, "==") == 0) return registers[regIndex].value == value;
    if (strcmp(op, "!=") == 0) return registers[regIndex].value != value;
    return 0;
}

void executeInstruction(int regIndex, char* op, int value) {
    if (strcmp(op, "inc") == 0) registers[regIndex].value += value;
    else if (strcmp(op, "dec") == 0) registers[regIndex].value -= value;
}

int findMaxValue() {
    int max = 0;
    for (int i = 0; i < regCount; i++) {
        if (registers[i].value > max) {
            max = registers[i].value;
        }
    }
    return max;
}

int main() {
    FILE *file;
    char line[LINE_LENGTH];
    file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }

    while (fgets(line, sizeof(line), file)) {
        char reg[10], op[4], condReg[10], condOp[3];
        int value, condValue;
        sscanf(line, "%s %s %d if %s %s %d", reg, op, &value, condReg, condOp, &condValue);
        
        int regIndex = getRegisterIndex(reg);
        int condRegIndex = getRegisterIndex(condReg);
        
        if (checkCondition(condRegIndex, condOp, condValue)) {
            executeInstruction(regIndex, op, value);
        }
    }
    fclose(file);

    printf("%d\n", findMaxValue());
    return 0;
}
