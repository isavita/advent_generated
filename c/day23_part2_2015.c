
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 100
#define MAX_LINE_LEN 50

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char instructions[MAX_LINES][MAX_LINE_LEN];
    int num_instructions = 0;
    while (num_instructions < MAX_LINES && fgets(instructions[num_instructions], MAX_LINE_LEN, file)) {
        // Remove trailing newline character if present
        instructions[num_instructions][strcspn(instructions[num_instructions], "\n")] = 0;
        if (strlen(instructions[num_instructions]) > 0) { // Avoid empty lines
             num_instructions++;
        }
    }
    fclose(file);

    if (num_instructions == 0 && feof(file) == 0) {
         fprintf(stderr, "Error reading file or file is empty/too large.\n");
         return 1;
    }


    unsigned long long reg_a = 1;
    unsigned long long reg_b = 0;
    int ip = 0; // Instruction pointer

    while (ip >= 0 && ip < num_instructions) {
        char *line = instructions[ip];
        char instruction[4];
        char reg_char = 0;
        int offset = 0;
        unsigned long long *reg_ptr = NULL;

        // Extract the 3-letter instruction code
        sscanf(line, "%3s", instruction);

        if (strcmp(instruction, "hlf") == 0) {
            sscanf(line + 4, "%c", &reg_char);
            reg_ptr = (reg_char == 'a') ? &reg_a : &reg_b;
            *reg_ptr /= 2;
            ip++;
        } else if (strcmp(instruction, "tpl") == 0) {
            sscanf(line + 4, "%c", &reg_char);
            reg_ptr = (reg_char == 'a') ? &reg_a : &reg_b;
            *reg_ptr *= 3;
            ip++;
        } else if (strcmp(instruction, "inc") == 0) {
            sscanf(line + 4, "%c", &reg_char);
            reg_ptr = (reg_char == 'a') ? &reg_a : &reg_b;
            (*reg_ptr)++;
            ip++;
        } else if (strcmp(instruction, "jmp") == 0) {
            sscanf(line + 4, "%d", &offset);
            ip += offset;
        } else if (strcmp(instruction, "jie") == 0) {
            sscanf(line + 4, "%c, %d", &reg_char, &offset);
            reg_ptr = (reg_char == 'a') ? &reg_a : &reg_b;
            if (*reg_ptr % 2 == 0) {
                ip += offset;
            } else {
                ip++;
            }
        } else if (strcmp(instruction, "jio") == 0) {
            sscanf(line + 4, "%c, %d", &reg_char, &offset);
            reg_ptr = (reg_char == 'a') ? &reg_a : &reg_b;
            if (*reg_ptr == 1) {
                ip += offset;
            } else {
                ip++;
            }
        } else {
            fprintf(stderr, "Unknown instruction: %s at line %d\n", line, ip);
            return 1; // Error on unknown instruction
        }
    }

    printf("%llu\n", reg_b);

    return 0;
}
