
#include <stdio.h>
#include <stdlib.h>

int compare(const void *a, const void *b) {
    return (*(int*)b - *(int*)a);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    int caloriesList[1000];
    int currentCalories = 0;
    int index = 0;
    char line[255];

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n') {
            caloriesList[index++] = currentCalories;
            currentCalories = 0;
            continue;
        }

        int calories = atoi(line);
        currentCalories += calories;
    }

    caloriesList[index++] = currentCalories;
    qsort(caloriesList, index, sizeof(int), compare);

    int topThreeSum = 0;
    for (int i = 0; i < 3 && i < index; i++) {
        topThreeSum += caloriesList[i];
    }

    fclose(file);

    printf("%d\n", topThreeSum);
    return 0;
}
