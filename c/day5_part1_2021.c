#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y;
} point;

#define SIZE 1000

int grid[SIZE][SIZE] = {0};

int main() {
    FILE *file = fopen("input.txt", "r");
    char line[50];
    while (fgets(line, sizeof(line), file)) {
        int x1, y1, x2, y2;
        sscanf(line, "%d,%d -> %d,%d", &x1, &y1, &x2, &y2);
        if (x1 == x2) {
            if (y1 > y2) { int temp = y1; y1 = y2; y2 = temp; }
            for (int y = y1; y <= y2; y++) grid[x1][y]++;
        } else if (y1 == y2) {
            if (x1 > x2) { int temp = x1; x1 = x2; x2 = temp; }
            for (int x = x1; x <= x2; x++) grid[x][y1]++;
        }
    }
    fclose(file);

    int overlapCount = 0;
    for (int i = 0; i < SIZE; i++)
        for (int j = 0; j < SIZE; j++)
            if (grid[i][j] > 1) overlapCount++;

    printf("%d\n", overlapCount);
    return 0;
}