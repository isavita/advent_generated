
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INSTRUCTIONS 1000
#define MAX_LEN 10

typedef struct Instruction {
    char operation[MAX_LEN];
    int argument;
} Instruction;

int main() {
    FILE *fp;
    char line[MAX_LEN];
    Instruction instructions[MAX_INSTRUCTIONS];
    int visited[MAX_INSTRUCTIONS] = {0};
    int accumulator = 0;
    int index = 0;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    int num_instructions = 0;
    while (fgets(line, sizeof(line), fp)) {
        sscanf(line, "%s %d", instructions[num_instructions].operation, &instructions[num_instructions].argument);
        num_instructions++;
    }

    fclose(fp);

    while (index < num_instructions) {
        if (visited[index]) {
            break;
        }
        visited[index] = 1;

        if (strcmp(instructions[index].operation, "acc") == 0) {
            accumulator += instructions[index].argument;
            index++;
        } else if (strcmp(instructions[index].operation, "jmp") == 0) {
            index += instructions[index].argument;
        } else if (strcmp(instructions[index].operation, "nop") == 0) {
            index++;
        }
    }

    printf("%d\n", accumulator);

    return 0;
}
