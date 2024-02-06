
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX_COORDINATES 50
#define MAX_GRID_SIZE 500

typedef struct {
    int x;
    int y;
} Coordinate;

int manhattanDistance(int x1, int y1, int x2, int y2) {
    return abs(x1 - x2) + abs(y1 - y2);
}

int main() {
    Coordinate coordinates[MAX_COORDINATES];
    int numCoordinates = 0;
    int minX = INT_MAX, minY = INT_MAX, maxX = INT_MIN, maxY = INT_MIN;

    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return -1;
    }

    while (fscanf(file, "%d, %d\n", &coordinates[numCoordinates].x, &coordinates[numCoordinates].y) != EOF) {
        if (coordinates[numCoordinates].x < minX) {
            minX = coordinates[numCoordinates].x;
        }
        if (coordinates[numCoordinates].x > maxX) {
            maxX = coordinates[numCoordinates].x;
        }
        if (coordinates[numCoordinates].y < minY) {
            minY = coordinates[numCoordinates].y;
        }
        if (coordinates[numCoordinates].y > maxY) {
            maxY = coordinates[numCoordinates].y;
        }
        numCoordinates++;
    }

    int grid[MAX_GRID_SIZE][MAX_GRID_SIZE] = {0};
    int areas[MAX_COORDINATES] = {0};
    int infiniteAreas[MAX_COORDINATES] = {0};

    int sizeLargestArea = 0;

    for (int x = minX; x <= maxX; x++) {
        for (int y = minY; y <= maxY; y++) {
            int minDistance = INT_MAX;
            int closestCoordinate = -1;
            int totalDistance = 0;

            for (int i = 0; i < numCoordinates; i++) {
                int distance = manhattanDistance(x, y, coordinates[i].x, coordinates[i].y);
                totalDistance += distance;

                if (distance < minDistance) {
                    minDistance = distance;
                    closestCoordinate = i;
                } else if (distance == minDistance) {
                    closestCoordinate = -1;
                }
            }

            if (closestCoordinate != -1) {
                areas[closestCoordinate]++;
                grid[x][y] = closestCoordinate;
                if (x == minX || x == maxX || y == minY || y == maxY) {
                    infiniteAreas[closestCoordinate] = 1;
                }
            }

            if (totalDistance < 10000) {
                sizeLargestArea++;
            }
        }
    }

    int largestArea = 0;
    for (int i = 0; i < numCoordinates; i++) {
        if (areas[i] > largestArea && !infiniteAreas[i]) {
            largestArea = areas[i];
        }
    }

    printf("%d\n", largestArea);

    fclose(file);

    return 0;
}
