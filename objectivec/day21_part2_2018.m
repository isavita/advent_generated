
#import <Foundation/Foundation.h>

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

int addr(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]] + regs[abc[1]]; return 0; }
int addi(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]] + abc[1]; return 0; }
int mulr(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]] * regs[abc[1]]; return 0; }
int muli(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]] * abc[1]; return 0; }
int banr(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]] & regs[abc[1]]; return 0; }
int bani(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]] & abc[1]; return 0; }
int borr(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]] | regs[abc[1]]; return 0; }
int bori(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]] | abc[1]; return 0; }
int setr(int *regs, int *abc) { regs[abc[2]] = regs[abc[0]]; return 0; }
int seti(int *regs, int *abc) { regs[abc[2]] = abc[0]; return 0; }
int gtir(int *regs, int *abc) { regs[abc[2]] = (abc[0] > regs[abc[1]]) ? 1 : 0; return 0; }
int gtri(int *regs, int *abc) { regs[abc[2]] = (regs[abc[0]] > abc[1]) ? 1 : 0; return 0; }
int gtrr(int *regs, int *abc) { regs[abc[2]] = (regs[abc[0]] > regs[abc[1]]) ? 1 : 0; return 0; }
int eqir(int *regs, int *abc) { regs[abc[2]] = (abc[0] == regs[abc[1]]) ? 1 : 0; return 0; }
int eqri(int *regs, int *abc) { regs[abc[2]] = (regs[abc[0]] == abc[1]) ? 1 : 0; return 0; }
int eqrr(int *regs, int *abc) { regs[abc[2]] = (regs[abc[0]] == regs[abc[1]]) ? 1 : 0; return 0; }

typedef int (*OpcodeFunc)(int *, int *);

static OpcodeFunc opcodeFuncs[16] = {
    addr, addi, mulr, muli, banr, bani, borr, bori,
    setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
};

static const char *opcodeNames[16] = {
    "addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori",
    "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"
};

int getOpcodeIndex(const char *name) {
    for (int i = 0; i < 16; i++)
        if (strcmp(name, opcodeNames[i]) == 0) return i;
    return -1;
}

bool tick(OpcodeComputer *o) {
    if (o->registers[o->instructionPointer] >= o->numInstructions) return true;
    int ip = o->registers[o->instructionPointer];
    Instruction inst = o->instructions[ip];
    int idx = getOpcodeIndex(inst.name);
    if (idx == -1) return true;
    opcodeFuncs[idx](o->registers, inst.abcValues);
    o->registers[o->instructionPointer]++;
    return o->registers[o->instructionPointer] >= o->numInstructions;
}

OpcodeComputer parseInput(const char *input) {
    OpcodeComputer oc; oc.instructions = NULL; oc.numInstructions = 0;
    for (int i = 0; i < 6; i++) oc.registers[i] = 0;
    char *buf = strdup(input);
    char *line = strtok(buf, "\n");
    sscanf(line, "#ip %d", &oc.instructionPointer);
    while ((line = strtok(NULL, "\n")) != NULL) {
        oc.instructions = realloc(oc.instructions, (oc.numInstructions + 1) * sizeof(Instruction));
        sscanf(line, "%4s %d %d %d", oc.instructions[oc.numInstructions].name,
               &oc.instructions[oc.numInstructions].abcValues[0],
               &oc.instructions[oc.numInstructions].abcValues[1],
               &oc.instructions[oc.numInstructions].abcValues[2]);
        oc.numInstructions++;
    }
    free(buf);
    return oc;
}

int solve(const char *input) {
    OpcodeComputer oc = parseInput(input);
    int lastReg5 = 0;
    bool *seen = calloc(100000, sizeof(bool));
    while (!tick(&oc)) {
        if (oc.registers[oc.instructionPointer] == 28) {
            int r5 = oc.registers[5];
            if (seen[r5]) break;
            seen[r5] = true;
            lastReg5 = r5;
        }
    }
    free(seen);
    free(oc.instructions);
    return lastReg5;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&err];
        if (err) { perror([[err localizedDescription] UTF8String]); return 1; }
        int result = solve([content UTF8String]);
        printf("%d\n", result);
    }
    return 0;
}
