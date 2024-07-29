#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int calculateFuel(int currentPosition, int newPosition) {
    return abs(currentPosition - newPosition);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) return 1;

    int *positions = malloc(1000 * sizeof(int));
    if (!positions) return 1; // Check for memory allocation failure
    int count = 0;
    char line[10000];
    
    while (fgets(line, sizeof(line), file)) {
        char *token = strtok(line, ",");
        while (token) {
            positions[count++] = atoi(token);
            token = strtok(NULL, ",");
        }
    }
    fclose(file);

    int min_fuel = __INT_MAX__;
    int max_position = positions[0];

    for (int i = 1; i < count; i++) {
        if (positions[i] > max_position) {
            max_position = positions[i];
        }
    }

    for (int i = 0; i <= max_position; i++) {
        int fuel = 0;
        for (int j = 0; j < count; j++) {
            fuel += calculateFuel(positions[j], i);
        }
        if (fuel < min_fuel) {
            min_fuel = fuel;
        }
    }

    printf("%d\n", min_fuel);
    free(positions); // Free allocated memory
    return 0;
}