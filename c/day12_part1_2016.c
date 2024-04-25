#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INSTRUCTIONS 100
#define MAX_REGISTER_NAME 2
#define MAX_INSTRUCTION_LENGTH 10

typedef struct {
    char name[MAX_REGISTER_NAME];
    int value;
} Register;

void executeInstructions(char instructions[][MAX_INSTRUCTION_LENGTH], Register registers[4], int instructionCount) {
    int i = 0;
    while (i < instructionCount) {
        char instruction[MAX_INSTRUCTION_LENGTH];
        strcpy(instruction, instructions[i]);
        char* parts[3];
        char* token = strtok(instruction, " ");
        int j = 0;
        while (token != NULL) {
            parts[j++] = token;
            token = strtok(NULL, " ");
        }
        if (strcmp(parts[0], "cpy") == 0) {
            int val;
            if (strcmp(parts[1], "a") == 0) {
                val = registers[0].value;
            } else if (strcmp(parts[1], "b") == 0) {
                val = registers[1].value;
            } else if (strcmp(parts[1], "c") == 0) {
                val = registers[2].value;
            } else if (strcmp(parts[1], "d") == 0) {
                val = registers[3].value;
            } else {
                val = atoi(parts[1]);
            }
            if (strcmp(parts[2], "a") == 0) {
                registers[0].value = val;
            } else if (strcmp(parts[2], "b") == 0) {
                registers[1].value = val;
            } else if (strcmp(parts[2], "c") == 0) {
                registers[2].value = val;
            } else if (strcmp(parts[2], "d") == 0) {
                registers[3].value = val;
            }
            i++;
        } else if (strcmp(parts[0], "inc") == 0) {
            if (strcmp(parts[1], "a") == 0) {
                registers[0].value++;
            } else if (strcmp(parts[1], "b") == 0) {
                registers[1].value++;
            } else if (strcmp(parts[1], "c") == 0) {
                registers[2].value++;
            } else if (strcmp(parts[1], "d") == 0) {
                registers[3].value++;
            }
            i++;
        } else if (strcmp(parts[0], "dec") == 0) {
            if (strcmp(parts[1], "a") == 0) {
                registers[0].value--;
            } else if (strcmp(parts[1], "b") == 0) {
                registers[1].value--;
            } else if (strcmp(parts[1], "c") == 0) {
                registers[2].value--;
            } else if (strcmp(parts[1], "d") == 0) {
                registers[3].value--;
            }
            i++;
        } else if (strcmp(parts[0], "jnz") == 0) {
            int val;
            if (strcmp(parts[1], "a") == 0) {
                val = registers[0].value;
            } else if (strcmp(parts[1], "b") == 0) {
                val = registers[1].value;
            } else if (strcmp(parts[1], "c") == 0) {
                val = registers[2].value;
            } else if (strcmp(parts[1], "d") == 0) {
                val = registers[3].value;
            } else {
                val = atoi(parts[1]);
            }
            if (val != 0) {
                i += atoi(parts[2]);
            } else {
                i++;
            }
        }
    }
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char instructions[MAX_INSTRUCTIONS][MAX_INSTRUCTION_LENGTH];
    int instructionCount = 0;
    while (fgets(instructions[instructionCount], MAX_INSTRUCTION_LENGTH, file)) {
        instructions[instructionCount][strcspn(instructions[instructionCount], "\n")] = '\0';
        instructionCount++;
    }

    fclose(file);

    Register registers[4] = {{"a", 0}, {"b", 0}, {"c", 0}, {"d", 0}};
    executeInstructions(instructions, registers, instructionCount);

    printf("%d\n", registers[0].value);

    return 0;
}