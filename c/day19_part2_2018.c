
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PROGRAM_LENGTH 1024
#define MAX_REGISTERS 6

typedef int (*operation)(int r[], int a, int b);

int addr(int r[], int a, int b) { return r[a] + r[b]; }
int addi(int r[], int a, int b) { return r[a] + b; }
int mulr(int r[], int a, int b) { return r[a] * r[b]; }
int muli(int r[], int a, int b) { return r[a] * b; }
int banr(int r[], int a, int b) { return r[a] & r[b]; }
int bani(int r[], int a, int b) { return r[a] & b; }
int borr(int r[], int a, int b) { return r[a] | r[b]; }
int bori(int r[], int a, int b) { return r[a] | b; }
int setr(int r[], int a, int b) { return r[a]; }
int seti(int r[], int a, int b) { return a; }
int gtir(int r[], int a, int b) { return a > r[b] ? 1 : 0; }
int gtri(int r[], int a, int b) { return r[a] > b ? 1 : 0; }
int gtrr(int r[], int a, int b) { return r[a] > r[b] ? 1 : 0; }
int eqir(int r[], int a, int b) { return a == r[b] ? 1 : 0; }
int eqri(int r[], int a, int b) { return r[a] == b ? 1 : 0; }
int eqrr(int r[], int a, int b) { return r[a] == r[b] ? 1 : 0; }

operation ops[] = {addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr};
char* opcodes[] = {"addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"};

typedef struct {
    operation op;
    int a;
    int b;
    int c;
} Instruction;

int findOpcodeIndex(char* opcode) {
    for (int i = 0; i < 16; i++) {
        if (strcmp(opcodes[i], opcode) == 0) {
            return i;
        }
    }
    return -1; // Should not happen
}

void runProgram(int ipRegister, Instruction program[], int programLength, int* registers, int maxCycles) {
    int ip = 0;
    int cycles = 0;

    while (ip >= 0 && ip < programLength) {
        registers[ipRegister] = ip;
        Instruction inst = program[ip];
        registers[inst.c] = inst.op(registers, inst.a, inst.b);
        ip = registers[ipRegister] + 1;
        cycles++;
        if (maxCycles > 0 && cycles >= maxCycles) {
            break;
        }
    }
}

int max(int* slice, int length) {
    int maxValue = slice[0];
    for (int i = 1; i < length; i++) {
        if (slice[i] > maxValue) {
            maxValue = slice[i];
        }
    }
    return maxValue;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Failed to open input.txt");
        return 1;
    }

    char line[256];
    int ipRegister = 0;
    Instruction program[MAX_PROGRAM_LENGTH];
    int programLength = 0;

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '#') {
            sscanf(line, "#ip %d", &ipRegister);
            continue;
        }

        char opcode[5];
        int a, b, c;
        sscanf(line, "%s %d %d %d", opcode, &a, &b, &c);
        int opcodeIndex = findOpcodeIndex(opcode);
        program[programLength++] = (Instruction){ops[opcodeIndex], a, b, c};
    }

    fclose(file);

    int registers[MAX_REGISTERS] = {1, 0, 0, 0, 0, 0};
    runProgram(ipRegister, program, programLength, registers, 1000);

    int n = max(registers, MAX_REGISTERS);
    int total = 0;
    for (int i = 1; i <= n; i++) {
        if (n % i == 0) {
            total += i;
        }
    }

    printf("%d\n", total);

    return 0;
}
