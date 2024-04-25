#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GRID_SIZE 300

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        return 1;
    }

    int serial;
    fscanf(file, "%d", &serial);
    fclose(file);

    int grid[GRID_SIZE][GRID_SIZE];
    for (int y = 0; y < GRID_SIZE; y++) {
        for (int x = 0; x < GRID_SIZE; x++) {
            int rackID = x + 11;
            int powerLevel = rackID * (y + 1);
            powerLevel += serial;
            powerLevel *= rackID;
            powerLevel = (powerLevel / 100) % 10;
            powerLevel -= 5;
            grid[y][x] = powerLevel;
        }
    }

    int sum[GRID_SIZE + 1][GRID_SIZE + 1];
    for (int y = 0; y <= GRID_SIZE; y++) {
        for (int x = 0; x <= GRID_SIZE; x++) {
            sum[y][x] = 0;
        }
    }

    for (int y = 1; y <= GRID_SIZE; y++) {
        for (int x = 1; x <= GRID_SIZE; x++) {
            sum[y][x] = grid[y - 1][x - 1] + sum[y - 1][x] + sum[y][x - 1] - sum[y - 1][x - 1];
        }
    }

    int maxPower = -2147483648;
    int maxX, maxY, maxSize;
    for (int size = 1; size <= GRID_SIZE; size++) {
        for (int y = 1; y <= GRID_SIZE - size + 1; y++) {
            for (int x = 1; x <= GRID_SIZE - size + 1; x++) {
                int totalPower = sum[y + size - 1][x + size - 1] - sum[y + size - 1][x - 1] - sum[y - 1][x + size - 1] + sum[y - 1][x - 1];
                if (totalPower > maxPower) {
                    maxPower = totalPower;
                    maxX = x;
                    maxY = y;
                    maxSize = size;
                }
            }
        }
    }

    printf("%d,%d,%d\n", maxX, maxY, maxSize);

    return 0;
}