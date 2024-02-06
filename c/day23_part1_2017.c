
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INSTRUCTIONS 1000
#define MAX_LINE_LENGTH 50

int getValue(char* s, int* registers) {
    if (s[0] >= 'a' && s[0] <= 'z') {
        return registers[s[0] - 'a'];
    } else {
        return atoi(s);
    }
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char instructions[MAX_INSTRUCTIONS][MAX_LINE_LENGTH];
    int registers[26] = {0}; // Assuming registers a-z
    int mulCount = 0;
    int pointer = 0;
    int instructionCount = 0;

    while (fgets(instructions[instructionCount], MAX_LINE_LENGTH, file) != NULL) {
        instructions[instructionCount][strcspn(instructions[instructionCount], "\n")] = 0; // Remove newline
        instructionCount++;
    }

    fclose(file);

    while (pointer >= 0 && pointer < instructionCount) {
        char cmd[4], x[10], y[10];
        sscanf(instructions[pointer], "%s %s %s", cmd, x, y);

        if (strcmp(cmd, "set") == 0) {
            registers[x[0] - 'a'] = getValue(y, registers);
        } else if (strcmp(cmd, "sub") == 0) {
            registers[x[0] - 'a'] -= getValue(y, registers);
        } else if (strcmp(cmd, "mul") == 0) {
            registers[x[0] - 'a'] *= getValue(y, registers);
            mulCount++;
        } else if (strcmp(cmd, "jnz") == 0) {
            if (getValue(x, registers) != 0) {
                pointer += getValue(y, registers) - 1;
            }
        }
        pointer++;
    }

    printf("%d\n", mulCount);

    return 0;
}
