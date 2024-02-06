
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char a, b, action;
    char name[5];
    unsigned char matchCount[256];
} OP;

int strToInt(char *s) {
    return atoi(s);
}

void add(OP *op, unsigned char c) {
    if (strchr(op->matchCount, c) == NULL) {
        int len = strlen(op->matchCount);
        op->matchCount[len] = c;
        op->matchCount[len + 1] = '\0';
    }
}

int match(int r[], int c[], int len) {
    for (int i = 0; i < len; i++) {
        if (r[i] != c[i]) {
            return 0;
        }
    }
    return 1;
}

void runOp(OP op, int registers[], unsigned char instruction[], int result[]) {
    int A, B;
    A = (op.a == 'r') ? registers[instruction[1]] : instruction[1];
    B = (op.b == 'r') ? registers[instruction[2]] : instruction[2];
    
    switch (op.action) {
        case '+':
            result[instruction[3]] = A + B;
            break;
        case '*':
            result[instruction[3]] = A * B;
            break;
        case '&':
            result[instruction[3]] = A & B;
            break;
        case '|':
            result[instruction[3]] = A | B;
            break;
        case 'a':
            result[instruction[3]] = A;
            break;
        case '>':
            result[instruction[3]] = A > B;
            break;
        case '=':
            result[instruction[3]] = A == B;
            break;
        default:
            printf("Invalid instruction\n");
    }
}

int testCode(int registers[], int n[], unsigned char instruction[], OP opcodes[], int opCount) {
    int sum = 0, result[4];
    for (int i = 0; i < opCount; i++) {
        memcpy(result, registers, sizeof(int) * 4); // Copy registers to result
        runOp(opcodes[i], registers, instruction, result);
        if (match(n, result, 4)) {
            add(&opcodes[i], instruction[0]);
            sum++;
        }
    }
    return sum;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    OP opcodes[] = {
        {'r', 'r', '+', "addr"},
        {'r', 'v', '+', "addi"},
        {'r', 'r', '*', "mulr"},
        {'r', 'v', '*', "muli"},
        {'r', 'r', '&', "banr"},
        {'r', 'v', '&', "bani"},
        {'r', 'r', '|', "borr"},
        {'r', 'v', '|', "bori"},
        {'r', 'r', 'a', "setr"},
        {'v', 'r', 'a', "seti"},
        {'v', 'r', '>', "gtir"},
        {'r', 'v', '>', "gtri"},
        {'r', 'r', '>', "gtrr"},
        {'v', 'r', '=', "eqir"},
        {'r', 'v', '=', "eqri"},
        {'r', 'r', '=', "eqir"}
    };
    int opCount = sizeof(opcodes) / sizeof(opcodes[0]);
    for (int i = 0; i < opCount; i++) {
        opcodes[i].matchCount[0] = '\0'; // Initialize matchCount as empty string
    }

    char line[256];
    int sum = 0;
    while (fgets(line, sizeof(line), file)) {
        if (line[0] == 'B') {
            int registers[4], n[4];
            unsigned char instruction[4];
            sscanf(line, "Before: [%d, %d, %d, %d]", &registers[0], &registers[1], &registers[2], &registers[3]);
            fgets(line, sizeof(line), file);
            sscanf(line, "%hhu %hhu %hhu %hhu", &instruction[0], &instruction[1], &instruction[2], &instruction[3]);
            fgets(line, sizeof(line), file);
            sscanf(line, "After: [%d, %d, %d, %d]", &n[0], &n[1], &n[2], &n[3]);
            
            int tempSum = testCode(registers, n, instruction, opcodes, opCount);
            if (tempSum >= 3) {
                sum++;
            }
            fgets(line, sizeof(line), file); // Skip empty line
        }
    }

    printf("%d\n", sum);
    fclose(file);
    return 0;
}
