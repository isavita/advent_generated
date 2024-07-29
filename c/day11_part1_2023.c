#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int X, Y;
} Coord;

typedef struct {
    int Width, Height;
    unsigned char *Data;
} Grid;

unsigned char Empty = '.';

Grid buildGrid(char **input, int height, int width) {
    Grid grid = { width, height, calloc(width * height, sizeof(unsigned char)) };
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (input[y][x] != Empty) {
                grid.Data[y * width + x] = input[y][x];
            }
        }
    }
    return grid;
}

int isEmptyRow(Grid grid, int y) {
    for (int x = 0; x < grid.Width; x++) {
        if (grid.Data[y * grid.Width + x] != 0) return 0;
    }
    return 1;
}

int isEmptyCol(Grid grid, int x) {
    for (int y = 0; y < grid.Height; y++) {
        if (grid.Data[y * grid.Width + x] != 0) return 0;
    }
    return 1;
}

void calculateOffsets(int *emptyIndexes, int emptyCount, int *offsets, int bound) {
    for (int i = 0; i < emptyCount; i++) {
        for (int j = emptyIndexes[i] + 1; j < bound; j++) {
            offsets[j]++;
        }
    }
}

Grid expandGrid(Grid grid, int expansionFactor) {
    int emptyRows[grid.Height], emptyCols[grid.Width];
    int numEmptyRows = 0, numEmptyCols = 0;

    for (int y = 0; y < grid.Height; y++) {
        if (isEmptyRow(grid, y)) emptyRows[numEmptyRows++] = y;
    }
    for (int x = 0; x < grid.Width; x++) {
        if (isEmptyCol(grid, x)) emptyCols[numEmptyCols++] = x;
    }

    Grid newGrid = { grid.Width + numEmptyCols * (expansionFactor - 1), grid.Height + numEmptyRows * (expansionFactor - 1), calloc((grid.Width + numEmptyCols * (expansionFactor - 1)) * (grid.Height + numEmptyRows * (expansionFactor - 1)), sizeof(unsigned char)) };

    int dXs[grid.Width], dYs[grid.Height];
    memset(dXs, 0, sizeof(dXs));
    memset(dYs, 0, sizeof(dYs));

    calculateOffsets(emptyCols, numEmptyCols, dXs, grid.Width);
    calculateOffsets(emptyRows, numEmptyRows, dYs, grid.Height);

    for (int y = 0; y < grid.Height; y++) {
        for (int x = 0; x < grid.Width; x++) {
            if (grid.Data[y * grid.Width + x] != 0) {
                int newX = x + dXs[x] * (expansionFactor - 1);
                int newY = y + dYs[y] * (expansionFactor - 1);
                newGrid.Data[newY * newGrid.Width + newX] = grid.Data[y * grid.Width + x];
            }
        }
    }
    return newGrid;
}

int calculateLength(Coord c1, Coord c2) {
    return abs(c2.X - c1.X) + abs(c2.Y - c1.Y);
}

int solve(char **input, int height, int width) {
    Grid grid = buildGrid(input, height, width);
    Grid expandedGrid = expandGrid(grid, 2);
    
    int res = 0;
    Coord *seen = malloc(expandedGrid.Width * expandedGrid.Height * sizeof(Coord));
    int seenCount = 0;

    for (int y1 = 0; y1 < expandedGrid.Height; y1++) {
        for (int x1 = 0; x1 < expandedGrid.Width; x1++) {
            if (expandedGrid.Data[y1 * expandedGrid.Width + x1] != 0) {
                for (int i = 0; i < seenCount; i++) {
                    res += calculateLength((Coord){x1, y1}, seen[i]);
                }
                seen[seenCount++] = (Coord){x1, y1};
            }
        }
    }

    free(grid.Data);
    free(expandedGrid.Data);
    free(seen);
    return res;
}

char **readFile(const char *fileName, int *height, int *width) {
    FILE *file = fopen(fileName, "r");
    char **input = NULL;
    char line[1024];
    *height = 0;
    *width = 0;

    while (fgets(line, sizeof(line), file)) {
        input = realloc(input, (*height + 1) * sizeof(char *));
        input[*height] = strdup(line);
        input[*height][strcspn(input[*height], "\n")] = 0;
        if (*width < strlen(input[*height])) *width = strlen(input[*height]);
        (*height)++;
    }
    fclose(file);
    return input;
}

int main() {
    int height, width;
    char **input = readFile("input.txt", &height, &width);
    printf("%d\n", solve(input, height, width));
    for (int i = 0; i < height; i++) free(input[i]);
    free(input);
    return 0;
}