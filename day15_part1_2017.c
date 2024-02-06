
#include <stdio.h>

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

    for (int i = 0; i < 40000000; i++) {
        genA = (genA * genAFactor) % modulus;
        genB = (genB * genBFactor) % modulus;

        if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
            matches++;
        }
    }

    printf("%d\n", matches);

    fclose(file);
    return 0;
}
