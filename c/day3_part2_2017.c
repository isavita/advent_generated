
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    int input;
    fscanf(file, "%d", &input);
    fclose(file);

    int layer = 0, size = 1, steps = 0;
    while (size * size < input) {
        layer++;
        size += 2;
    }
    int cycle = size - 1;
    int offset = input - (size - 2) * (size - 2);
    offset = offset % cycle;
    steps = layer + abs(offset - layer);

    printf("%d\n", steps);

    int x = 0, y = 0, dx = 0, dy = -1, t, val = 1, maxLayer = 10;
    int spiral[1000][1000] = {0};
    spiral[500][500] = 1; // Initial value at center

    while (val <= input) {
        if ((x == y) || (x < 0 && x == -y) || (x > 0 && x == 1-y)) {
            t = dx; dx = -dy; dy = t; // Change direction
        }
        x += dx; y += dy;
        val = 0;
        for (int i = -1; i <= 1; i++) {
            for (int j = -1; j <= 1; j++) {
                val += spiral[500 + x + i][500 + y + j];
            }
        }
        spiral[500 + x][500 + y] = val;
        if (x > maxLayer || y > maxLayer || x < -maxLayer || y < -maxLayer) break; // Safety break
    }

    printf("%d\n", val);
    return 0;
}
