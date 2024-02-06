
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int abs(int x) {
    return x < 0 ? -x : x;
}

int sign(int x) {
    return (x > 0) - (x < 0);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Failed to open input file");
        return 1;
    }

    int x1, y1, x2, y2;
    int grid[1000][1000] = {0}; // Assuming the grid size based on problem constraints
    while (fscanf(file, "%d,%d -> %d,%d\n", &x1, &y1, &x2, &y2) == 4) {
        int xStep = sign(x2 - x1);
        int yStep = sign(y2 - y1);
        int steps = abs(x2-x1) > abs(y2-y1) ? abs(x2-x1) : abs(y2-y1);

        for (int i = 0; i <= steps; i++) {
            grid[y1 + i*yStep][x1 + i*xStep]++;
        }
    }
    fclose(file);

    int count = 0;
    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < 1000; j++) {
            if (grid[i][j] > 1) {
                count++;
            }
        }
    }

    printf("%d\n", count);

    return 0;
}
