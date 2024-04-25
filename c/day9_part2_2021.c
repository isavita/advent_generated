#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 100

int compare(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int isLowPoint(int **heightmap, int x, int y, int rows, int cols) {
    int height = heightmap[y][x];
    if (x > 0 && heightmap[y][x-1] <= height) return 0;
    if (x < cols-1 && heightmap[y][x+1] <= height) return 0;
    if (y > 0 && heightmap[y-1][x] <= height) return 0;
    if (y < rows-1 && heightmap[y+1][x] <= height) return 0;
    return 1;
}

int exploreBasin(int **heightmap, int x, int y, int rows, int cols, int *visited) {
    if (visited[y*cols+x] || heightmap[y][x] == 9) return 0;
    visited[y*cols+x] = 1;
    int size = 1;

    int directions[4][2] = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}};
    for (int i = 0; i < 4; i++) {
        int newX = x + directions[i][0];
        int newY = y + directions[i][1];
        if (newX >= 0 && newX < cols && newY >= 0 && newY < rows) {
            size += exploreBasin(heightmap, newX, newY, rows, cols, visited);
        }
    }
    return size;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Could not open file\n");
        return 1;
    }

    int rows = 0;
    int cols = 0;
    int **heightmap = NULL;
    char line[1024];
    while (fgets(line, 1024, file)) {
        if (cols == 0) {
            cols = strlen(line) - 1;
            heightmap = (int**)malloc(sizeof(int*) * MAX_SIZE);
            for (int i = 0; i < MAX_SIZE; i++) {
                heightmap[i] = (int*)malloc(sizeof(int) * cols);
            }
        }
        for (int i = 0; i < cols; i++) {
            heightmap[rows][i] = line[i] - '0';
        }
        rows++;
    }

    int *basinSizes = (int*)malloc(sizeof(int) * MAX_SIZE);
    int basinSizeCount = 0;

    int *visited = (int*)calloc(rows*cols, sizeof(int));

    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            if (isLowPoint(heightmap, x, y, rows, cols)) {
                int size = exploreBasin(heightmap, x, y, rows, cols, visited);
                basinSizes[basinSizeCount++] = size;
            }
        }
    }

    qsort(basinSizes, basinSizeCount, sizeof(int), compare);

    int result = 1;
    for (int i = 0; i < 3; i++) {
        result *= basinSizes[basinSizeCount-1-i];
    }
    printf("%d\n", result);

    for (int i = 0; i < rows; i++) {
        free(heightmap[i]);
    }
    free(heightmap);
    free(basinSizes);
    free(visited);
    fclose(file);
    return 0;
}