
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int getValue(int* program, int pos, int mode) {
    if (mode == 0) {
        return program[program[pos]];
    } else {
        return program[pos];
    }
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    int program[1000]; // Adjust size as necessary
    int programSize = 0;
    char line[5000]; // Adjust size as necessary

    if (fgets(line, sizeof(line), file)) {
        char* token = strtok(line, ",");
        while (token != NULL) {
            program[programSize++] = atoi(token);
            token = strtok(NULL, ",");
        }
    }
    fclose(file);

    int input = 5;
    int output = 0;
    int i = 0;

    while (1) {
        int opcode = program[i] % 100;
        int modes = program[i] / 100;
        int param1Mode = modes % 10;
        modes /= 10;
        int param2Mode = modes % 10;

        switch (opcode) {
            case 1: {
                int p1 = getValue(program, i + 1, param1Mode);
                int p2 = getValue(program, i + 2, param2Mode);
                int p3 = program[i + 3];
                program[p3] = p1 + p2;
                i += 4;
                break;
            }
            case 2: {
                int p1 = getValue(program, i + 1, param1Mode);
                int p2 = getValue(program, i + 2, param2Mode);
                int p3 = program[i + 3];
                program[p3] = p1 * p2;
                i += 4;
                break;
            }
            case 3:
                program[program[i + 1]] = input;
                i += 2;
                break;
            case 4:
                output = getValue(program, i + 1, param1Mode);
                printf("%d\n", output);
                i += 2;
                break;
            case 5: {
                int p1 = getValue(program, i + 1, param1Mode);
                int p2 = getValue(program, i + 2, param2Mode);
                if (p1 != 0) {
                    i = p2;
                } else {
                    i += 3;
                }
                break;
            }
            case 6: {
                int p1 = getValue(program, i + 1, param1Mode);
                int p2 = getValue(program, i + 2, param2Mode);
                if (p1 == 0) {
                    i = p2;
                } else {
                    i += 3;
                }
                break;
            }
            case 7: {
                int p1 = getValue(program, i + 1, param1Mode);
                int p2 = getValue(program, i + 2, param2Mode);
                int p3 = program[i + 3];
                if (p1 < p2) {
                    program[p3] = 1;
                } else {
                    program[p3] = 0;
                }
                i += 4;
                break;
            }
            case 8: {
                int p1 = getValue(program, i + 1, param1Mode);
                int p2 = getValue(program, i + 2, param2Mode);
                int p3 = program[i + 3];
                if (p1 == p2) {
                    program[p3] = 1;
                } else {
                    program[p3] = 0;
                }
                i += 4;
                break;
            }
            case 99:
                return EXIT_SUCCESS;
            default:
                fprintf(stderr, "Invalid opcode\n");
                return EXIT_FAILURE;
        }
    }
}
