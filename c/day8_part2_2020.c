
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int executeBootCode(char **instructions, int numInstructions);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char **instructions = NULL;
    char buffer[256];
    int numInstructions = 0;

    while (fgets(buffer, sizeof(buffer), file) != NULL) {
        instructions = realloc(instructions, (numInstructions + 1) * sizeof(char *));
        instructions[numInstructions] = strdup(buffer);
        numInstructions++;
    }

    for (int i = 0; i < numInstructions; i++) {
        char op[4];
        int arg;
        sscanf(instructions[i], "%s %d", op, &arg);
        if (strcmp(op, "acc") == 0) {
            continue;
        }

        char **modifiedInstructions = malloc(numInstructions * sizeof(char *));
        for (int j = 0; j < numInstructions; j++) {
            modifiedInstructions[j] = strdup(instructions[j]);
        }

        if (strcmp(op, "jmp") == 0) {
            sprintf(modifiedInstructions[i], "nop %d", arg);
        } else {
            sprintf(modifiedInstructions[i], "jmp %d", arg);
        }

        int accumulator = executeBootCode(modifiedInstructions, numInstructions);
        if (accumulator != -1) {
            printf("%d\n", accumulator);
            break;
        }

        for (int j = 0; j < numInstructions; j++) {
            free(modifiedInstructions[j]);
        }
        free(modifiedInstructions);
    }

    for (int i = 0; i < numInstructions; i++) {
        free(instructions[i]);
    }
    free(instructions);

    fclose(file);
    return 0;
}

int executeBootCode(char **instructions, int numInstructions) {
    int accumulator = 0;
    int *visited = calloc(numInstructions, sizeof(int));
    int currentInstruction = 0;

    while (currentInstruction < numInstructions) {
        if (visited[currentInstruction]) {
            free(visited);
            return -1;
        }

        visited[currentInstruction] = 1;

        char op[4];
        int arg;
        sscanf(instructions[currentInstruction], "%s %d", op, &arg);

        if (strcmp(op, "acc") == 0) {
            accumulator += arg;
            currentInstruction++;
        } else if (strcmp(op, "jmp") == 0) {
            currentInstruction += arg;
        } else {
            currentInstruction++;
        }
    }

    free(visited);
    return accumulator;
}
