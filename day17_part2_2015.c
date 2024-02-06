
#include <stdio.h>
#include <stdlib.h>

#define MAX_CONTAINERS 100

void findCombinations(int containers[], int target, int index, int count, int *minCount, int *ways, int totalContainers) {
    if (target == 0) {
        if (*minCount == 0 || count < *minCount) {
            *minCount = count;
            *ways = 1;
        } else if (count == *minCount) {
            (*ways)++;
        }
        return;
    }
    if (target < 0 || index >= totalContainers) {
        return;
    }
    // Include current container
    findCombinations(containers, target - containers[index], index + 1, count + 1, minCount, ways, totalContainers);
    // Exclude current container
    findCombinations(containers, target, index + 1, count, minCount, ways, totalContainers);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int containers[MAX_CONTAINERS];
    int size, totalContainers = 0;
    while (fscanf(file, "%d", &size) == 1) {
        containers[totalContainers++] = size;
    }
    fclose(file);

    int minCount = 0, ways = 0;
    findCombinations(containers, 150, 0, 0, &minCount, &ways, totalContainers);
    printf("%d\n", ways);

    return 0;
}
