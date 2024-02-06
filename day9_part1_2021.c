
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 256

int isLowPoint(int **heightmap, int x, int y, int maxX, int maxY) {
    int height = heightmap[y][x];
    if (x > 0 && heightmap[y][x-1] <= height) {
        return 0;
    }
    if (x < maxX - 1 && heightmap[y][x+1] <= height) {
        return 0;
    }
    if (y > 0 && heightmap[y-1][x] <= height) {
        return 0;
    }
    if (y < maxY - 1 && heightmap[y+1][x] <= height) {
        return 0;
    }
    return 1;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    int **heightmap = NULL;
    int rows = 0, cols = 0;

    while (fgets(line, MAX_LINE_LENGTH, file) != NULL) {
        if (heightmap == NULL) {
            cols = strlen(line) - 1; // -1 to exclude newline character
            heightmap = (int **)malloc(sizeof(int *));
        } else {
            heightmap = (int **)realloc(heightmap, (rows + 1) * sizeof(int *));
        }
        heightmap[rows] = (int *)malloc(cols * sizeof(int));
        
        for (int i = 0; i < cols; i++) {
            heightmap[rows][i] = line[i] - '0'; // Convert char to int
        }
        rows++;
    }
    fclose(file);

    int totalRiskLevel = 0;
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            if (isLowPoint(heightmap, x, y, cols, rows)) {
                totalRiskLevel += 1 + heightmap[y][x];
            }
        }
    }

    printf("%d\n", totalRiskLevel);

    // Free allocated memory
    for (int i = 0; i < rows; i++) {
        free(heightmap[i]);
    }
    free(heightmap);

    return 0;
}
