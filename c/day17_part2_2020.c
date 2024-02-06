
#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 30
#define CYCLES 6

int countActive(int grid[MAX_SIZE][MAX_SIZE][MAX_SIZE][MAX_SIZE], int x, int y, int z, int w) {
    int count = 0;
    for (int i = x - 1; i <= x + 1; i++) {
        for (int j = y - 1; j <= y + 1; j++) {
            for (int k = z - 1; k <= z + 1; k++) {
                for (int l = w - 1; l <= w + 1; l++) {
                    if (i == x && j == y && k == z && l == w) continue;
                    count += grid[i][j][k][l];
                }
            }
        }
    }
    return count;
}

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    int input[MAX_SIZE][MAX_SIZE] = {0};
    int grid[MAX_SIZE][MAX_SIZE][MAX_SIZE][MAX_SIZE] = {0};

    int mid = MAX_SIZE / 2;
    int offset = mid - 3;

    for (int i = offset; i < offset + 8; i++) {
        for (int j = offset; j < offset + 8; j++) {
            char c;
            fscanf(fp, " %c", &c);
            input[i][j] = c == '#';
        }
    }
    fclose(fp);

    for (int i = offset; i < offset + 8; i++) {
        for (int j = offset; j < offset + 8; j++) {
            grid[mid][mid][i][j] = input[i][j];
        }
    }

    for (int cycle = 0; cycle < CYCLES; cycle++) {
        int newGrid[MAX_SIZE][MAX_SIZE][MAX_SIZE][MAX_SIZE] = {0};
        for (int x = 1; x < MAX_SIZE - 1; x++) {
            for (int y = 1; y < MAX_SIZE - 1; y++) {
                for (int z = 1; z < MAX_SIZE - 1; z++) {
                    for (int w = 1; w < MAX_SIZE - 1; w++) {
                        int activeNeighbors = countActive(grid, x, y, z, w);
                        if (grid[x][y][z][w] == 1) {
                            newGrid[x][y][z][w] = (activeNeighbors == 2 || activeNeighbors == 3);
                        } else {
                            newGrid[x][y][z][w] = (activeNeighbors == 3);
                        }
                    }
                }
            }
        }
        for (int x = 1; x < MAX_SIZE - 1; x++) {
            for (int y = 1; y < MAX_SIZE - 1; y++) {
                for (int z = 1; z < MAX_SIZE - 1; z++) {
                    for (int w = 1; w < MAX_SIZE - 1; w++) {
                        grid[x][y][z][w] = newGrid[x][y][z][w];
                    }
                }
            }
        }
    }

    int count = 0;
    for (int x = 0; x < MAX_SIZE; x++) {
        for (int y = 0; y < MAX_SIZE; y++) {
            for (int z = 0; z < MAX_SIZE; z++) {
                for (int w = 0; w < MAX_SIZE; w++) {
                    count += grid[x][y][z][w];
                }
            }
        }
    }

    printf("%d\n", count);

    return 0;
}
