
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    int maxCalories = 0, currentCalories = 0, calories;
    char line[256];

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n') {
            if (currentCalories > maxCalories) {
                maxCalories = currentCalories;
            }
            currentCalories = 0;
        } else {
            calories = atoi(line);
            currentCalories += calories;
        }
    }

    if (currentCalories > maxCalories) {
        maxCalories = currentCalories;
    }

    fclose(file);
    printf("%d\n", maxCalories);
    return 0;
}
