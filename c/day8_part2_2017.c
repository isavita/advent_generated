
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

#define MAX_REGISTERS 100
#define MAX_LINE 256

typedef struct {
    char name[10];
    int value;
} Register;

int findRegister(Register registers[], int numRegisters, const char *name) {
    for (int i = 0; i < numRegisters; ++i) {
        if (strcmp(registers[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Register registers[MAX_REGISTERS];
    int numRegisters = 0;
    int highestValue = 0;
    char line[MAX_LINE];

    while (fgets(line, sizeof(line), file) != NULL) {
        char reg[10], op[4], condReg[10], condOp[3];
        int amount, condVal;
        sscanf(line, "%s %s %d if %s %s %d", reg, op, &amount, condReg, condOp, &condVal);

        int regIndex = findRegister(registers, numRegisters, reg);
        if (regIndex == -1) {
            if(numRegisters == MAX_REGISTERS){
                fclose(file);
                return 1;
            }
            strcpy(registers[numRegisters].name, reg);
            registers[numRegisters].value = 0;
            regIndex = numRegisters++;
        }
         int condRegIndex = findRegister(registers, numRegisters, condReg);
        if (condRegIndex == -1) {
            if(numRegisters == MAX_REGISTERS){
                fclose(file);
                return 1;
            }
             strcpy(registers[numRegisters].name, condReg);
            registers[numRegisters].value = 0;
            condRegIndex = numRegisters++;
         }

        bool condition = false;
        if (strcmp(condOp, ">") == 0) {
            condition = registers[condRegIndex].value > condVal;
        } else if (strcmp(condOp, ">=") == 0) {
            condition = registers[condRegIndex].value >= condVal;
        } else if (strcmp(condOp, "<") == 0) {
            condition = registers[condRegIndex].value < condVal;
        } else if (strcmp(condOp, "<=") == 0) {
            condition = registers[condRegIndex].value <= condVal;
        } else if (strcmp(condOp, "==") == 0) {
            condition = registers[condRegIndex].value == condVal;
        } else if (strcmp(condOp, "!=") == 0) {
            condition = registers[condRegIndex].value != condVal;
        }

        if (condition) {
            if (strcmp(op, "inc") == 0) {
                registers[regIndex].value += amount;
            } else if (strcmp(op, "dec") == 0) {
                registers[regIndex].value -= amount;
            }

            if (registers[regIndex].value > highestValue) {
                highestValue = registers[regIndex].value;
            }
        }
    }

    fclose(file);
    printf("%d\n", highestValue);
    return 0;
}
