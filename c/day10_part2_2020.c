#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int compare(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

long long countArrangements(int *adapters, int n) {
    long long ways[n];
    memset(ways, 0, sizeof(ways));
    ways[0] = 1;

    for (int i = 1; i < n; i++) {
        for (int j = 1; j <= 3; j++) {
            if (i - j >= 0 && adapters[i] - adapters[i - j] <= 3) {
                ways[i] += ways[i - j];
            }
        }
    }

    return ways[n - 1];
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    int adapters[1000];
    int n = 1;
    adapters[0] = 0;

    while (fscanf(file, "%d", &adapters[n]) == 1) {
        n++;
    }

    qsort(adapters, n, sizeof(int), compare);
    adapters[n] = adapters[n - 1] + 3;
    n++;

    printf("%lld\n", countArrangements(adapters, n));

    fclose(file);
    return 0;
}