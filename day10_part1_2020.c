
#include <stdio.h>
#include <stdlib.h>

int compare(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int adapters[1000];
    int numAdapters = 0;
    int joltage, previousJoltage = 0;

    while (fscanf(file, "%d", &joltage) != EOF) {
        adapters[numAdapters++] = joltage;
    }

    qsort(adapters, numAdapters, sizeof(int), compare);

    int joltDifferences[4] = {0};
    joltDifferences[3] = 1;

    for (int i = 0; i < numAdapters; i++) {
        int diff = adapters[i] - previousJoltage;
        joltDifferences[diff]++;
        previousJoltage = adapters[i];
    }

    int product = joltDifferences[1] * joltDifferences[3];
    printf("%d\n", product);

    fclose(file);
    return 0;
}
