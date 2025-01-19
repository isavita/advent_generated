
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdint.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    int packages[100];
    int numPackages = 0;
    int totalWeight = 0;
    char line[20];

    while (fgets(line, sizeof(line), file) != NULL) {
        packages[numPackages] = atoi(line);
        totalWeight += packages[numPackages];
        numPackages++;
    }
    fclose(file);
    
    int targetWeight = totalWeight / 3;
    uint64_t bestQE = UINT64_MAX;
    int bestLength = INT_MAX;

    for (int comb = 1; comb < (1 << numPackages); comb++) {
        int groupWeight = 0;
        uint64_t qe = 1;
        int groupLength = 0;

        for (int i = 0; i < numPackages; i++) {
            if (comb & (1 << i)) {
                groupWeight += packages[i];
                qe *= (uint64_t)packages[i];
                groupLength++;
            }
        }
        
         if (groupWeight == targetWeight && groupLength <= bestLength) {
            if (groupLength < bestLength || qe < bestQE) {
                bestLength = groupLength;
                bestQE = qe;
            }
        }
    }

    printf("%llu\n", bestQE);

    return 0;
}
