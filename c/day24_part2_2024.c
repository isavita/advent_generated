
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
static int gate_count = 0;

char* find_output_by_gate(const char* a1, const char* op, const char* b1) {
    for (int i = 0; i < gate_count; i++) {
        Gate* g = &gates[i];
        if (strcmp(g->gate.op, op) == 0) {
            if ((strcmp(g->gate.a, a1) == 0 && strcmp(g->gate.b, b1) == 0) ||
                (strcmp(g->gate.a, b1) == 0 && strcmp(g->gate.b, a1) == 0)) {
                return g->output;
            }
        }
    }
    return NULL;
}

Gate* find_gate_by_output(const char* output) {
    for (int i = 0; i < gate_count; i++) {
        if (strcmp(gates[i].output, output) == 0) {
            return &gates[i];
        }
    }
    return NULL;
}

void swap_gate_outputs(const char* out1, const char* out2) {
    char name1[MAX_NAME_LEN], name2[MAX_NAME_LEN];
    strcpy(name1, out1);
    strcpy(name2, out2);
    for (int i = 0; i < gate_count; i++) {
        if (strcmp(gates[i].output, name1) == 0) {
            strcpy(gates[i].output, name2);
        } else if (strcmp(gates[i].output, name2) == 0) {
            strcpy(gates[i].output, name1);
        }
    }
}

int compare_strings(const void* a, const void* b) {
    return strcmp((const char*)a, (const char*)b);
}

int main(void) {
    FILE* file = fopen("input.txt", "rb");
    if (!file) return 1;

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char* buffer = malloc(size + 1);
    fread(buffer, 1, size, file);
    buffer[size] = 0;
    fclose(file);

    char* definitions = strstr(buffer, "\n\n");
    if (definitions) definitions += 2;
    else definitions = buffer;
    
    int numZ = 0;
    char* line = strtok(definitions, "\n");
    while (line && *line) {
        Gate* g = &gates[gate_count++];
        sscanf(line, "%s %s %s -> %s", g->gate.a, g->gate.op, g->gate.b, g->output);
        if (g->output[0] == 'z') numZ++;
        line = strtok(NULL, "\n");
    }
    free(buffer);

    char swapped_pairs[NUM_PAIRS * 2][MAX_NAME_LEN];
    int pair_count = 0;

    while (pair_count < NUM_PAIRS) {
        char* carry = "";
        for (int i = 0; i < numZ; i++) {
            char xi[MAX_NAME_LEN], yi[MAX_NAME_LEN], zi[MAX_NAME_LEN];
            sprintf(xi, "x%02d", i);
            sprintf(yi, "y%02d", i);
            sprintf(zi, "z%02d", i);

            char* adder = NULL;
            char* next_carry = NULL;

            if (i == 0) {
                adder = find_output_by_gate(xi, "XOR", yi);
                next_carry = find_output_by_gate(xi, "AND", yi);
            } else {
                char* bit = find_output_by_gate(xi, "XOR", yi);
                if (bit && carry && *carry) {
                    adder = find_output_by_gate(bit, "XOR", carry);
                    if (adder) {
                        char* c1 = find_output_by_gate(xi, "AND", yi);
                        char* c2 = find_output_by_gate(bit, "AND", carry);
                        if (c1 && c2) {
                            next_carry = find_output_by_gate(c1, "OR", c2);
                        }
                    }
                }
            }
            
            int swapped = 0;
            if (adder && strcmp(adder, zi) != 0) {
                strcpy(swapped_pairs[pair_count * 2], adder);
                strcpy(swapped_pairs[pair_count * 2 + 1], zi);
                swap_gate_outputs(adder, zi);
                swapped = 1;
            } else if (!adder) {
                Gate* gate_z = find_gate_by_output(zi);
                char* bit = find_output_by_gate(xi, "XOR", yi);
                if (gate_z && bit && carry && *carry) {
                     if (find_output_by_gate(gate_z->gate.a, "XOR", carry)) {
                        strcpy(swapped_pairs[pair_count * 2], bit);
                        strcpy(swapped_pairs[pair_count * 2 + 1], gate_z->gate.a);
                        swap_gate_outputs(bit, gate_z->gate.a);
                        swapped = 1;
                    } else if (find_output_by_gate(gate_z->gate.b, "XOR", carry)) {
                        strcpy(swapped_pairs[pair_count * 2], bit);
                        strcpy(swapped_pairs[pair_count * 2 + 1], gate_z->gate.b);
                        swap_gate_outputs(bit, gate_z->gate.b);
                        swapped = 1;
                    }
                }
            }

            if (swapped) {
                pair_count++;
                break;
            }
            carry = next_carry ? next_carry : "";
        }
    }

    qsort(swapped_pairs, NUM_PAIRS * 2, MAX_NAME_LEN, compare_strings);

    for (int i = 0; i < NUM_PAIRS * 2; i++) {
        printf("%s%s", swapped_pairs[i], (i == NUM_PAIRS * 2 - 1) ? "" : ",");
    }
    printf("\n");

    return 0;
}
