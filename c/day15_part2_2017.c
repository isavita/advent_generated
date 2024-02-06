
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error reading file\n");
        return 1;
    }

    long genAStart, genBStart;
    fscanf(file, "%ld", &genAStart);
    fscanf(file, "%ld", &genBStart);

    long genAFactor = 16807;
    long genBFactor = 48271;
    long modulus = 2147483647;

    long genA = genAStart;
    long genB = genBStart;
    int matches = 0;

    for (int i = 0; i < 5000000; i++) {
        // Generate next value for A that is a multiple of 4
        do {
            genA = (genA * genAFactor) % modulus;
        } while (genA % 4 != 0);

        // Generate next value for B that is a multiple of 8
        do {
            genB = (genB * genBFactor) % modulus;
        } while (genB % 8 != 0);

        if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
            matches++;
        }
    }

    printf("%d\n", matches);

    fclose(file);
    return 0;
}
