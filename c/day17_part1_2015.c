
#include <stdio.h>
#include <stdlib.h>

#define MAX_CONTAINERS 100
#define TARGET 150

int countCombinations(int containers[], int target, int index, int size) {
    if (target == 0) {
        return 1;
    }
    if (target < 0 || index >= size) {
        return 0;
    }
    // Include current container and exclude current container
    return countCombinations(containers, target - containers[index], index + 1, size) +
           countCombinations(containers, target, index + 1, size);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file.\n");
        return 1;
    }

    int containers[MAX_CONTAINERS];
    int size = 0;

    while (fscanf(file, "%d", &containers[size]) == 1) {
        size++;
    }

    fclose(file);

    printf("%d\n", countCombinations(containers, TARGET, 0, size));

    return 0;
}
