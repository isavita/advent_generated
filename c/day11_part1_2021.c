#include <stdio.h>
#include <stdlib.h>

#define SIZE 10

int grid[SIZE][SIZE];
int flashed[SIZE][SIZE];

void readInput(const char *filename) {
    FILE *file = fopen(filename, "r");
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            fscanf(file, "%1d", &grid[i][j]);
            flashed[i][j] = 0;
        }
    }
    fclose(file);
}

int flash(int x, int y) {
    if (flashed[y][x]) return 0;
    flashed[y][x] = 1;
    int flashes = 1;
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            if (dx == 0 && dy == 0) continue;
            int newX = x + dx, newY = y + dy;
            if (newX >= 0 && newX < SIZE && newY >= 0 && newY < SIZE) {
                grid[newY][newX]++;
                if (grid[newY][newX] > 9) {
                    flashes += flash(newX, newY);
                }
            }
        }
    }
    return flashes;
}

int simulateStep() {
    int totalFlashes = 0;
    for (int y = 0; y < SIZE; y++) {
        for (int x = 0; x < SIZE; x++) {
            grid[y][x]++;
            flashed[y][x] = 0;
        }
    }
    for (int y = 0; y < SIZE; y++) {
        for (int x = 0; x < SIZE; x++) {
            if (grid[y][x] > 9) {
                totalFlashes += flash(x, y);
            }
        }
    }
    for (int y = 0; y < SIZE; y++) {
        for (int x = 0; x < SIZE; x++) {
            if (flashed[y][x]) grid[y][x] = 0;
        }
    }
    return totalFlashes;
}

int main() {
    readInput("input.txt");
    int totalFlashes = 0;
    for (int step = 0; step < 100; step++) {
        totalFlashes += simulateStep();
    }
    printf("%d\n", totalFlashes);
    return 0;
}