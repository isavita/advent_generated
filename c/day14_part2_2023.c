#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y;
} Coord;

typedef struct {
    int width, height;
    unsigned char *data;
} Grid;

#define EMPTY '.'
#define CUBIC_ROCK '#'
#define ROUND_ROCK 'O'

Coord add(Coord c1, Coord c2) {
    return (Coord){c1.x + c2.x, c1.y + c2.y};
}

int isInBounds(Coord coord, Grid grid) {
    return coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height;
}

void buildGrid(Grid *grid, const char **input, int height) {
    grid->width = strlen(input[0]);
    grid->height = height;
    grid->data = calloc(grid->width * grid->height, sizeof(unsigned char));
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < grid->width; x++) {
            if (input[y][x] != EMPTY) {
                grid->data[y * grid->width + x] = input[y][x];
            }
        }
    }
}

void shiftSingleRock(Grid *grid, Coord coord, Coord dir) {
    if (grid->data[coord.y * grid->width + coord.x] == ROUND_ROCK) {
        Coord current = coord;
        Coord before = add(coord, dir);
        while (isInBounds(before, *grid) && grid->data[before.y * grid->width + before.x] == 0) {
            grid->data[before.y * grid->width + before.x] = ROUND_ROCK;
            grid->data[current.y * grid->width + current.x] = 0;
            current = before;
            before = add(before, dir);
        }
    }
}

void shiftRocks(Grid *grid, Coord dir) {
    if (dir.y < 0 || dir.x < 0) {
        for (int x = 0; x < grid->width; x++) {
            for (int y = 0; y < grid->height; y++) {
                shiftSingleRock(grid, (Coord){x, y}, dir);
            }
        }
    } else {
        for (int x = grid->width - 1; x >= 0; x--) {
            for (int y = grid->height - 1; y >= 0; y--) {
                shiftSingleRock(grid, (Coord){x, y}, dir);
            }
        }
    }
}

void cycleRocks(Grid *grid) {
    shiftRocks(grid, (Coord){0, -1});
    shiftRocks(grid, (Coord){-1, 0});
    shiftRocks(grid, (Coord){0, 1});
    shiftRocks(grid, (Coord){1, 0});
}

int calculateGridKey(Grid grid) {
    int key = 0;
    for (int x = 0; x < grid.width; x++) {
        for (int y = 0; y < grid.height; y++) {
            if (grid.data[y * grid.width + x] == ROUND_ROCK) {
                key += x + y * grid.width;
            }
        }
    }
    return key;
}

int calculateLoad(Grid grid) {
    int load = 0;
    for (int x = 0; x < grid.width; x++) {
        for (int y = 0; y < grid.height; y++) {
            if (grid.data[y * grid.width + x] == ROUND_ROCK) {
                load += grid.height - y;
            }
        }
    }
    return load;
}

int solve(const char **input, int height) {
    int numCycles = 1000000000;
    Grid grid;
    buildGrid(&grid, input, height);
    int *cache = calloc(numCycles, sizeof(int));
    memset(cache, -1, numCycles * sizeof(int));

    for (int i = 0; i < numCycles; i++) {
        int gridKey = calculateGridKey(grid);
        if (cache[gridKey] != -1) {
            int remainingCycles = (numCycles - cache[gridKey]) % (i - cache[gridKey]);
            for (int j = 0; j < remainingCycles; j++) {
                cycleRocks(&grid);
            }
            free(cache);
            free(grid.data);
            return calculateLoad(grid);
        }
        cache[gridKey] = i;
        cycleRocks(&grid);
    }

    free(cache);
    free(grid.data);
    return calculateLoad(grid);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    char **input = NULL;
    size_t size = 0;
    char buffer[256];
    while (fgets(buffer, sizeof(buffer), file)) {
        input = realloc(input, ++size * sizeof(char *));
        input[size - 1] = strdup(buffer);
        input[size - 1][strcspn(input[size - 1], "\n")] = 0;
    }
    fclose(file);
    int result = solve((const char **)input, size);
    for (size_t i = 0; i < size; i++) free(input[i]);
    free(input);
    printf("%d\n", result);
    return 0;
}