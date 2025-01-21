
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_WIRES 1000
#define MAX_GATES 1000
#define MAX_LINE_LENGTH 256

// Structure to represent a wire
typedef struct {
    char name[4];
    int value;
    int initialized;
} Wire;

// Structure to represent a gate
typedef struct {
    char input1[4];
    char input2[4];
    char output[4];
    char operation[4];
} Gate;

// Function to find a wire by name
Wire *find_wire(Wire *wires, int num_wires, const char *name) {
    for (int i = 0; i < num_wires; i++) {
        if (strcmp(wires[i].name, name) == 0) {
            return &wires[i];
        }
    }
    return NULL;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Wire wires[MAX_WIRES];
    Gate gates[MAX_GATES];
    int num_wires = 0;
    int num_gates = 0;
    char line[MAX_LINE_LENGTH];

    // Read initial wire values
    while (fgets(line, sizeof(line), file) != NULL) {
        if (strstr(line, "->") != NULL) break; 
        
        char name[4];
        int value;
        if (sscanf(line, "%3s: %d", name, &value) == 2) {
            strcpy(wires[num_wires].name, name);
            wires[num_wires].value = value;
            wires[num_wires].initialized = 1;
            num_wires++;
        }
    }

    // Read gate connections
     do {
        if (strstr(line, "->") == NULL) continue;
       
        char input1[4], operation[4], input2[4], output[4];
        if (sscanf(line, "%3s %3s %3s -> %3s", input1, operation, input2, output) == 4) {
            strcpy(gates[num_gates].input1, input1);
            strcpy(gates[num_gates].operation, operation);
            strcpy(gates[num_gates].input2, input2);
            strcpy(gates[num_gates].output, output);
            num_gates++;
        }
    } while (fgets(line, sizeof(line), file) != NULL);

    fclose(file);

    // Simulate the circuit
    int changed = 1;
    while (changed) {
        changed = 0;
        for (int i = 0; i < num_gates; i++) {
            Wire *in1 = find_wire(wires, num_wires, gates[i].input1);
            Wire *in2 = find_wire(wires, num_wires, gates[i].input2);
            Wire *out = find_wire(wires, num_wires, gates[i].output);

            if (in1 == NULL) {
                strcpy(wires[num_wires].name, gates[i].input1);
                wires[num_wires].initialized = 0;
                in1 = &wires[num_wires++];
            }
            if (in2 == NULL) {
                strcpy(wires[num_wires].name, gates[i].input2);
                wires[num_wires].initialized = 0;
                in2 = &wires[num_wires++];
            }
            if (out == NULL) {
                strcpy(wires[num_wires].name, gates[i].output);
                wires[num_wires].initialized = 0;
                out = &wires[num_wires++];
            }

            if (in1->initialized && in2->initialized && !out->initialized) {
                if (strcmp(gates[i].operation, "AND") == 0) {
                    out->value = in1->value & in2->value;
                } else if (strcmp(gates[i].operation, "OR") == 0) {
                    out->value = in1->value | in2->value;
                } else if (strcmp(gates[i].operation, "XOR") == 0) {
                    out->value = in1->value ^ in2->value;
                }
                out->initialized = 1;
                changed = 1;
            }
        }
    }

    // Calculate the output number
    long long output_num = 0;
    long long power_of_2 = 1;

    for(int i = 0; i < num_wires; i++) {
      if (strncmp(wires[i].name, "z", 1) == 0) {
        int wireNum = 0;
        for (int j = 1; isdigit(wires[i].name[j]); j++) {
          wireNum = wireNum * 10 + (wires[i].name[j] - '0');
        }
        
        if (wires[i].initialized) {
            if (wires[i].value) {
                long long temp_power_of_2 = 1;
                for (int k = 0; k < wireNum; k++)
                    temp_power_of_2 *= 2;
                output_num += temp_power_of_2;
            }
        }
      }
    }

    printf("%lld\n", output_num);

    return 0;
}
