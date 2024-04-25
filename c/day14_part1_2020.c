#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

uint64_t applyMask(uint64_t value, char *mask) {
    uint64_t result = 0;
    for (int i = 0; i < 36; i++) {
        uint64_t bitValue = 1ULL << (35 - i);
        if (mask[i] == '1') {
            result |= bitValue;
        } else if (mask[i] == 'X') {
            result |= (value & bitValue);
        }
    }
    return result;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char line[256];
    char mask[37] = "";
    uint64_t mem[100000] = {0}; // assuming at most 100000 memory addresses

    while (fgets(line, 256, file)) {
        if (strncmp(line, "mask = ", 7) == 0) {
            sscanf(line, "mask = %36s", mask);
        } else {
            int address, value;
            if (sscanf(line, "mem[%d] = %d", &address, &value) == 2) {
                mem[address] = applyMask(value, mask);
            }
        }
    }

    uint64_t sum = 0;
    for (int i = 0; i < 100000; i++) {
        sum += mem[i];
    }

    printf("%llu\n", sum);

    fclose(file);
    return 0;
}