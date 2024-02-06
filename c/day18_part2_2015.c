
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define gridSize 100
#define steps 100

int countOnNeighbors(bool grid[gridSize][gridSize], int x, int y) {
    int on = 0;
    for (int dx = -1; dx <= 1; dx++) {
        for (int dy = -1; dy <= 1; dy++) {
            if (dx == 0 && dy == 0) {
                continue;
            }
            int nx = x + dx, ny = y + dy;
            if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny]) {
                on++;
            }
        }
    }
    return on;
}

void step(bool grid[gridSize][gridSize], bool newGrid[gridSize][gridSize]) {
    for (int x = 0; x < gridSize; x++) {
        for (int y = 0; y < gridSize; y++) {
            int onNeighbors = countOnNeighbors(grid, x, y);
            if (grid[x][y]) {
                newGrid[x][y] = onNeighbors == 2 || onNeighbors == 3;
            } else {
                newGrid[x][y] = onNeighbors == 3;
            }
        }
    }

    // Ensure corners are always on
    newGrid[0][0] = true;
    newGrid[0][gridSize-1] = true;
    newGrid[gridSize-1][0] = true;
    newGrid[gridSize-1][gridSize-1] = true;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    bool grid[gridSize][gridSize] = {false};
    bool newGrid[gridSize][gridSize] = {false};
    char line[gridSize + 2]; // +2 for newline and null terminator
    int y = 0;

    while (fgets(line, sizeof(line), file) && y < gridSize) {
        for (int x = 0; x < gridSize && line[x] != '\n'; x++) {
            grid[x][y] = line[x] == '#';
        }
        y++;
    }

    fclose(file);

    // Initialize corners as always on
    grid[0][0] = true;
    grid[0][gridSize-1] = true;
    grid[gridSize-1][0] = true;
    grid[gridSize-1][gridSize-1] = true;

    for (int i = 0; i < steps; i++) {
        step(grid, newGrid);
        // Copy newGrid to grid
        for (int x = 0; x < gridSize; x++) {
            for (int y = 0; y < gridSize; y++) {
                grid[x][y] = newGrid[x][y];
            }
        }
    }

    int onCount = 0;
    for (int x = 0; x < gridSize; x++) {
        for (int y = 0; y < gridSize; y++) {
            if (grid[x][y]) {
                onCount++;
            }
        }
    }

    printf("%d\n", onCount);
    return 0;
}
