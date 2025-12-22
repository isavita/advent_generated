
#import <Foundation/Foundation.h>

#define MAX_GATES 256
#define MAX_NAME_LEN 8
#define NUM_PAIRS 4

typedef struct {
    char a[MAX_NAME_LEN];
    char op[4];
    char b[MAX_NAME_LEN];
} GateDef;

typedef struct {
    GateDef gate;
    char output[MAX_NAME_LEN];
} Gate;

static Gate gates[MAX_GATES];
static int gateCount = 0;

static char *findOutputByGate(const char *a1, const char *op, const char *b1) {
    for (int i = 0; i < gateCount; i++) {
        Gate *g = &gates[i];
        if (strcmp(g->gate.op, op) == 0) {
            if ((strcmp(g->gate.a, a1) == 0 && strcmp(g->gate.b, b1) == 0) ||
                (strcmp(g->gate.a, b1) == 0 && strcmp(g->gate.b, a1) == 0)) {
                return g->output;
            }
        }
    }
    return NULL;
}

static Gate *findGateByOutput(const char *output) {
    for (int i = 0; i < gateCount; i++) {
        if (strcmp(gates[i].output, output) == 0) {
            return &gates[i];
        }
    }
    return NULL;
}

static void swapGateOutputs(const char *out1, const char *out2) {
    char name1[MAX_NAME_LEN], name2[MAX_NAME_LEN];
    strcpy(name1, out1);
    strcpy(name2, out2);
    for (int i = 0; i < gateCount; i++) {
        if (strcmp(gates[i].output, name1) == 0) {
            strcpy(gates[i].output, name2);
        } else if (strcmp(gates[i].output, name2) == 0) {
            strcpy(gates[i].output, name1);
        }
    }
}

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                       encoding:NSUTF8StringEncoding
                                                          error:nil];
        if (!content) return 1;

        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        int numZ = 0;
        for (NSString *line in lines) {
            if ([line rangeOfString:@" -> "].location == NSNotFound) continue;
            Gate *g = &gates[gateCount++];
            sscanf([line UTF8String], "%s %s %s -> %s",
                   g->gate.a, g->gate.op, g->gate.b, g->output);
            if (g->output[0] == 'z') numZ++;
        }

        char swappedPairs[NUM_PAIRS * 2][MAX_NAME_LEN];
        int pairCount = 0;

        while (pairCount < NUM_PAIRS) {
            char *carry = "";
            for (int i = 0; i < numZ; i++) {
                char xi[MAX_NAME_LEN], yi[MAX_NAME_LEN], zi[MAX_NAME_LEN];
                sprintf(xi, "x%02d", i);
                sprintf(yi, "y%02d", i);
                sprintf(zi, "z%02d", i);

                char *adder = NULL;
                char *nextCarry = NULL;

                if (i == 0) {
                    adder = findOutputByGate(xi, "XOR", yi);
                    nextCarry = findOutputByGate(xi, "AND", yi);
                } else {
                    char *bit = findOutputByGate(xi, "XOR", yi);
                    if (bit && carry && *carry) {
                        adder = findOutputByGate(bit, "XOR", carry);
                        if (adder) {
                            char *c1 = findOutputByGate(xi, "AND", yi);
                            char *c2 = findOutputByGate(bit, "AND", carry);
                            if (c1 && c2) {
                                nextCarry = findOutputByGate(c1, "OR", c2);
                            }
                        }
                    }
                }

                int swapped = 0;
                if (adder && strcmp(adder, zi) != 0) {
                    strcpy(swappedPairs[pairCount * 2], adder);
                    strcpy(swappedPairs[pairCount * 2 + 1], zi);
                    swapGateOutputs(adder, zi);
                    swapped = 1;
                } else if (!adder) {
                    Gate *gateZ = findGateByOutput(zi);
                    char *bit = findOutputByGate(xi, "XOR", yi);
                    if (gateZ && bit && carry && *carry) {
                        if (findOutputByGate(gateZ->gate.a, "XOR", carry)) {
                            strcpy(swappedPairs[pairCount * 2], bit);
                            strcpy(swappedPairs[pairCount * 2 + 1], gateZ->gate.a);
                            swapGateOutputs(bit, gateZ->gate.a);
                            swapped = 1;
                        } else if (findOutputByGate(gateZ->gate.b, "XOR", carry)) {
                            strcpy(swappedPairs[pairCount * 2], bit);
                            strcpy(swappedPairs[pairCount * 2 + 1], gateZ->gate.b);
                            swapGateOutputs(bit, gateZ->gate.b);
                            swapped = 1;
                        }
                    }
                }

                if (swapped) {
                    pairCount++;
                    goto nextLoop;
                }
                carry = nextCarry ? nextCarry : "";
            }
            nextLoop:;
        }

        qsort(swappedPairs, NUM_PAIRS * 2, MAX_NAME_LEN, (int (*)(const void *, const void *))strcmp);

        NSMutableArray *result = [NSMutableArray array];
        for (int i = 0; i < NUM_PAIRS * 2; i++) {
            [result addObject:@(swappedPairs[i])];
        }
        printf("%s\n", [[result componentsJoinedByString:@","] UTF8String]);
    }
    return 0;
}
