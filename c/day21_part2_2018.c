
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    char name[5];
    int abcValues[3];
} Instruction;

typedef struct {
    Instruction *instructions;
    int numInstructions;
    int registers[6];
    int instructionPointer;
} OpcodeComputer;

typedef int (*OpcodeFunc)(int*, int*);

int addr(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]];
    return 0;
}

int addi(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1];
    return 0;
}

int mulr(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]];
    return 0;
}

int muli(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1];
    return 0;
}

int banr(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]];
    return 0;
}

int bani(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]] & abcValues[1];
    return 0;
}

int borr(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]];
    return 0;
}

int bori(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]] | abcValues[1];
    return 0;
}

int setr(int *registers, int *abcValues) {
    registers[abcValues[2]] = registers[abcValues[0]];
    return 0;
}

int seti(int *registers, int *abcValues) {
    registers[abcValues[2]] = abcValues[0];
    return 0;
}

int gtir(int *registers, int *abcValues) {
    registers[abcValues[2]] = (abcValues[0] > registers[abcValues[1]]) ? 1 : 0;
    return 0;
}

int gtri(int *registers, int *abcValues) {
    registers[abcValues[2]] = (registers[abcValues[0]] > abcValues[1]) ? 1 : 0;
    return 0;
}

int gtrr(int *registers, int *abcValues) {
    registers[abcValues[2]] = (registers[abcValues[0]] > registers[abcValues[1]]) ? 1 : 0;
    return 0;
}

int eqir(int *registers, int *abcValues) {
    registers[abcValues[2]] = (abcValues[0] == registers[abcValues[1]]) ? 1 : 0;
    return 0;
}

int eqri(int *registers, int *abcValues) {
    registers[abcValues[2]] = (registers[abcValues[0]] == abcValues[1]) ? 1 : 0;
    return 0;
}

int eqrr(int *registers, int *abcValues) {
    registers[abcValues[2]] = (registers[abcValues[0]] == registers[abcValues[1]]) ? 1 : 0;
    return 0;
}

OpcodeFunc opcodeNamesToFuncs[16] = {
    addr, addi, mulr, muli, banr, bani, borr, bori,
    setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
};
char* opcodeNames[] = {
    "addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori",
    "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"
};

int getOpcodeIndex(char *name){
    for (int i=0; i<16; i++){
        if(strcmp(name,opcodeNames[i]) == 0){
            return i;
        }
    }
    return -1;
}

bool tick(OpcodeComputer *o) {
    if (o->registers[o->instructionPointer] >= o->numInstructions) {
        return true;
    }
    int instIndex = o->registers[o->instructionPointer];
    Instruction inst = o->instructions[instIndex];

    int opcodeIndex = getOpcodeIndex(inst.name);
    if(opcodeIndex == -1){
        return true;
    }

    opcodeNamesToFuncs[opcodeIndex](o->registers, inst.abcValues);
    o->registers[o->instructionPointer]++;

    return o->registers[o->instructionPointer] >= o->numInstructions;
}

OpcodeComputer parseInput(char *input) {
    OpcodeComputer computer;
    computer.instructions = NULL;
    computer.numInstructions = 0;
    computer.registers[0] = 0;
    computer.registers[1] = 0;
    computer.registers[2] = 0;
    computer.registers[3] = 0;
    computer.registers[4] = 0;
    computer.registers[5] = 0;
    
    char *line = strtok(input, "\n");
    sscanf(line, "#ip %d", &computer.instructionPointer);
    line = strtok(NULL, "\n");
    while (line != NULL) {
        computer.instructions = realloc(computer.instructions, (computer.numInstructions + 1) * sizeof(Instruction));
        sscanf(line, "%4s %d %d %d", computer.instructions[computer.numInstructions].name,
               &computer.instructions[computer.numInstructions].abcValues[0],
               &computer.instructions[computer.numInstructions].abcValues[1],
               &computer.instructions[computer.numInstructions].abcValues[2]);
        computer.numInstructions++;
        line = strtok(NULL, "\n");
    }
    return computer;
}

int solve(char *input) {
    OpcodeComputer computer = parseInput(input);
    int lastReg5 = 0;
    int *comparedRegister5s = calloc(100000, sizeof(int));
    int count = 0;
    while (!tick(&computer)) {
        if (computer.registers[computer.instructionPointer] == 28) {
            int reg5 = computer.registers[5];
            if (comparedRegister5s[reg5] == 1) {
                break;
            }
            comparedRegister5s[reg5] = 1;
            lastReg5 = reg5;
            count++;
        }
    }
    free(comparedRegister5s);
    free(computer.instructions);
    return lastReg5;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }
    fseek(file, 0, SEEK_END);
    long filelen = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *input = (char *)malloc(filelen + 1);
    if (input == NULL) {
        fclose(file);
        perror("Memory allocation failed");
        return 1;
    }
    fread(input, 1, filelen, file);
    input[filelen] = '\0';
    fclose(file);

    printf("%d\n", solve(input));
    free(input);
    return 0;
}
