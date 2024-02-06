
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int toInt(char *s) {
    return atoi(s);
}

int boolToInt(int b) {
    return b ? 1 : 0;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    int ipBind;
    fscanf(file, "#ip %d\n", &ipBind);

    char instructions[1024][4][10];
    int numInstructions = 0;
    while (fscanf(file, "%s %s %s %s\n", instructions[numInstructions][0], instructions[numInstructions][1],
                  instructions[numInstructions][2], instructions[numInstructions][3]) != EOF) {
        numInstructions++;
    }

    int registers[6] = {0};
    for (int ip = 0; ip < numInstructions; ip++) {
        registers[ipBind] = ip;
        char *opcode = instructions[ip][0];
        int a = toInt(instructions[ip][1]);
        int b = toInt(instructions[ip][2]);
        int c = toInt(instructions[ip][3]);

        if (strcmp(opcode, "addr") == 0) {
            registers[c] = registers[a] + registers[b];
        } else if (strcmp(opcode, "addi") == 0) {
            registers[c] = registers[a] + b;
        } else if (strcmp(opcode, "mulr") == 0) {
            registers[c] = registers[a] * registers[b];
        } else if (strcmp(opcode, "muli") == 0) {
            registers[c] = registers[a] * b;
        } else if (strcmp(opcode, "banr") == 0) {
            registers[c] = registers[a] & registers[b];
        } else if (strcmp(opcode, "bani") == 0) {
            registers[c] = registers[a] & b;
        } else if (strcmp(opcode, "borr") == 0) {
            registers[c] = registers[a] | registers[b];
        } else if (strcmp(opcode, "bori") == 0) {
            registers[c] = registers[a] | b;
        } else if (strcmp(opcode, "setr") == 0) {
            registers[c] = registers[a];
        } else if (strcmp(opcode, "seti") == 0) {
            registers[c] = a;
        } else if (strcmp(opcode, "gtir") == 0) {
            registers[c] = boolToInt(a > registers[b]);
        } else if (strcmp(opcode, "gtri") == 0) {
            registers[c] = boolToInt(registers[a] > b);
        } else if (strcmp(opcode, "gtrr") == 0) {
            registers[c] = boolToInt(registers[a] > registers[b]);
        } else if (strcmp(opcode, "eqir") == 0) {
            registers[c] = boolToInt(a == registers[b]);
        } else if (strcmp(opcode, "eqri") == 0) {
            registers[c] = boolToInt(registers[a] == b);
        } else if (strcmp(opcode, "eqrr") == 0) {
            registers[c] = boolToInt(registers[a] == registers[b]);
        }

        ip = registers[ipBind];
        if (ip < 0 || ip >= numInstructions) {
            break;
        }
    }

    printf("%d\n", registers[0]);

    fclose(file);
    return 0;
}
