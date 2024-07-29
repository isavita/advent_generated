#include <stdio.h>
#include <stdlib.h>

#define SIZE 10

int grid[SIZE][SIZE];

void readInput(const char *filename) {
    FILE *file = fopen(filename, "r");
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            fscanf(file, "%1d", &grid[i][j]);
        }
    }
    fclose(file);
}

int flash(int x, int y, int flashed[SIZE][SIZE]) {
    if (flashed[y][x]) return 0;
    flashed[y][x] = 1;
    int flashes = 1;
    int directions[8][2] = {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}};
    
    for (int i = 0; i < 8; i++) {
        int newX = x + directions[i][0], newY = y + directions[i][1];
        if (newX >= 0 && newX < SIZE && newY >= 0 && newY < SIZE) {
            grid[newY][newX]++;
            if (grid[newY][newX] > 9) {
                flashes += flash(newX, newY, flashed);
            }
        }
    }
    return flashes;
}

int simulateStep() {
    int flashes = 0;
    int flashed[SIZE][SIZE] = {0};

    for (int y = 0; y < SIZE; y++) {
        for (int x = 0; x < SIZE; x++) {
            grid[y][x]++;
        }
    }

    for (int y = 0; y < SIZE; y++) {
        for (int x = 0; x < SIZE; x++) {
            if (grid[y][x] > 9) {
                flashes += flash(x, y, flashed);
            }
        }
    }

    for (int y = 0; y < SIZE; y++) {
        for (int x = 0; x < SIZE; x++) {
            if (flashed[y][x]) {
                grid[y][x] = 0;
            }
        }
    }

    return flashes;
}

int main() {
    readInput("input.txt");
    int step = 0;

    while (1) {
        step++;
        if (simulateStep() == 100) break;
    }

    printf("%d\n", step);
    return 0;
}