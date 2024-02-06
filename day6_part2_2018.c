
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX_COORDINATES 50

typedef struct {
    int x;
    int y;
} Coordinate;

int manhattanDistance(int x1, int y1, int x2, int y2) {
    return abs(x1 - x2) + abs(y1 - y2);
}

int main() {
    Coordinate coordinates[MAX_COORDINATES];
    int x, y, numCoordinates = 0;
    int minX = INT_MAX, minY = INT_MAX, maxX = INT_MIN, maxY = INT_MIN;

    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file.\n");
        return 1;
    }

    while (fscanf(file, "%d, %d\n", &x, &y) != EOF) {
        coordinates[numCoordinates].x = x;
        coordinates[numCoordinates].y = y;
        numCoordinates++;

        if (x < minX) minX = x;
        if (y < minY) minY = y;
        if (x > maxX) maxX = x;
        if (y > maxY) maxY = y;
    }

    fclose(file);

    int safeRegionSize = 0;

    for (int i = minX; i <= maxX; i++) {
        for (int j = minY; j <= maxY; j++) {
            int totalDistance = 0;
            for (int k = 0; k < numCoordinates; k++) {
                totalDistance += manhattanDistance(i, j, coordinates[k].x, coordinates[k].y);
            }
            if (totalDistance < 10000) {
                safeRegionSize++;
            }
        }
    }

    printf("%d\n", safeRegionSize);

    return 0;
}
