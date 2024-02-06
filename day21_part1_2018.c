
#include <stdio.h>
#include <string.h>

typedef struct {
    char name[5];
    int abcValues[3];
} Instruction;

typedef struct {
    Instruction instructions[50];
    int registers[6];
    int instructionPointer;
} OpcodeComputer;

OpcodeComputer parseInput(char *input) {
    OpcodeComputer opcodeComputer;
    char *line = strtok(input, "\n");

    sscanf(line, "#ip %d", &opcodeComputer.instructionPointer);

    int index = 0;
    while (line != NULL) {
        line = strtok(NULL, "\n");
        if (line != NULL) {
            sscanf(line, "%4s %d %d %d", opcodeComputer.instructions[index].name, 
                                        &opcodeComputer.instructions[index].abcValues[0], 
                                        &opcodeComputer.instructions[index].abcValues[1], 
                                        &opcodeComputer.instructions[index].abcValues[2]);
            index++;
        }
    }

    return opcodeComputer;
}

int addr(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]];
    return 0;
}

int addi(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1];
    return 0;
}

int mulr(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]];
    return 0;
}

int muli(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1];
    return 0;
}

int banr(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]];
    return 0;
}

int bani(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]] & abcValues[1];
    return 0;
}

int borr(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]];
    return 0;
}

int bori(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]] | abcValues[1];
    return 0;
}

int setr(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = registers[abcValues[0]];
    return 0;
}

int seti(int registers[6], int abcValues[3]) {
    registers[abcValues[2]] = abcValues[0];
    return 0;
}

int gtir(int registers[6], int abcValues[3]) {
    if (abcValues[0] > registers[abcValues[1]]) {
        registers[abcValues[2]] = 1;
    } else {
        registers[abcValues[2]] = 0;
    }
    return 0;
}

int gtri(int registers[6], int abcValues[3]) {
    if (registers[abcValues[0]] > abcValues[1]) {
        registers[abcValues[2]] = 1;
    } else {
        registers[abcValues[2]] = 0;
    }
    return 0;
}

int gtrr(int registers[6], int abcValues[3]) {
    if (registers[abcValues[0]] > registers[abcValues[1]]) {
        registers[abcValues[2]] = 1;
    } else {
        registers[abcValues[2]] = 0;
    }
    return 0;
}

int eqir(int registers[6], int abcValues[3]) {
    if (abcValues[0] == registers[abcValues[1]]) {
        registers[abcValues[2]] = 1;
    } else {
        registers[abcValues[2]] = 0;
    }
    return 0;
}

int eqri(int registers[6], int abcValues[3]) {
    if (registers[abcValues[0]] == abcValues[1]) {
        registers[abcValues[2]] = 1;
    } else {
        registers[abcValues[2]] = 0;
    }
    return 0;
}

int eqrr(int registers[6], int abcValues[3]) {
    if (registers[abcValues[0]] == registers[abcValues[1]]) {
        registers[abcValues[2]] = 1;
    } else {
        registers[abcValues[2]] = 0;
    }
    return 0;
}

int tick(OpcodeComputer *o) {
    if (o->registers[o->instructionPointer] >= sizeof(o->instructions) / sizeof(o->instructions[0])) {
        printf("Out of range instruction, terminating...\n");
        return 1;
    }

    int instIndex = o->registers[o->instructionPointer];
    Instruction inst = o->instructions[instIndex];

    int (*opcodeFunc)(int[6], int[3]) = NULL;

    if (strcmp(inst.name, "addr") == 0) {
        opcodeFunc = addr;
    } else if (strcmp(inst.name, "addi") == 0) {
        opcodeFunc = addi;
    } else if (strcmp(inst.name, "mulr") == 0) {
        opcodeFunc = mulr;
    } else if (strcmp(inst.name, "muli") == 0) {
        opcodeFunc = muli;
    } else if (strcmp(inst.name, "banr") == 0) {
        opcodeFunc = banr;
    } else if (strcmp(inst.name, "bani") == 0) {
        opcodeFunc = bani;
    } else if (strcmp(inst.name, "borr") == 0) {
        opcodeFunc = borr;
    } else if (strcmp(inst.name, "bori") == 0) {
        opcodeFunc = bori;
    } else if (strcmp(inst.name, "setr") == 0) {
        opcodeFunc = setr;
    } else if (strcmp(inst.name, "seti") == 0) {
        opcodeFunc = seti;
    } else if (strcmp(inst.name, "gtir") == 0) {
        opcodeFunc = gtir;
    } else if (strcmp(inst.name, "gtri") == 0) {
        opcodeFunc = gtri;
    } else if (strcmp(inst.name, "gtrr") == 0) {
        opcodeFunc = gtrr;
    } else if (strcmp(inst.name, "eqir") == 0) {
        opcodeFunc = eqir;
    } else if (strcmp(inst.name, "eqri") == 0) {
        opcodeFunc = eqri;
    } else if (strcmp(inst.name, "eqrr") == 0) {
        opcodeFunc = eqrr;
    }

    opcodeFunc(o->registers, inst.abcValues);

    o->registers[o->instructionPointer]++;

    if (o->registers[o->instructionPointer] >= sizeof(o->instructions) / sizeof(o->instructions[0])) {
        return 1;
    }

    return 0;
}

int solve(OpcodeComputer opcodeComputer) {
    while (!tick(&opcodeComputer)) {
        if (opcodeComputer.registers[opcodeComputer.instructionPointer] == 28) {
            break;
        }
    }

    return opcodeComputer.registers[5];
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char input[1000];
    fscanf(file, "%[^\0]", input);

    OpcodeComputer opcodeComputer = parseInput(input);
    printf("%d\n", solve(opcodeComputer));

    fclose(file);

    return 0;
}
