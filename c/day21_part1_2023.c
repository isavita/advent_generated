#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y;
} Coord;

typedef struct {
    int width, height;
    char **data;
} Grid;

Coord add(Coord c1, Coord c2) {
    return (Coord){c1.x + c2.x, c1.y + c2.y};
}

int isInBounds(Grid grid, Coord coord) {
    return coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height;
}

Grid parseInput(char **input, int height) {
    Grid grid = {strlen(input[0]), height, malloc(height * sizeof(char *))};
    for (int i = 0; i < height; i++) {
        grid.data[i] = malloc(grid.width);
        strcpy(grid.data[i], input[i]);
    }
    return grid;
}

Coord findStart(Grid grid) {
    for (int y = 0; y < grid.height; y++) {
        for (int x = 0; x < grid.width; x++) {
            if (grid.data[y][x] == 'S') return (Coord){x, y};
        }
    }
    exit(EXIT_FAILURE);
}

int bfs(Grid grid, Coord start, int *distances) {
    Coord directions[4] = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}};
    int queue[grid.width * grid.height];
    int front = 0, rear = 0;
    queue[rear++] = start.y * grid.width + start.x;
    distances[start.y * grid.width + start.x] = 0;

    while (front < rear) {
        int current = queue[front++];
        Coord currentCoord = {current % grid.width, current / grid.width};

        for (int i = 0; i < 4; i++) {
            Coord neighbor = add(currentCoord, directions[i]);
            if (isInBounds(grid, neighbor) && grid.data[neighbor.y][neighbor.x] != '#') {
                int index = neighbor.y * grid.width + neighbor.x;
                if (distances[index] == -1) {
                    distances[index] = distances[current] + 1;
                    queue[rear++] = index;
                }
            }
        }
    }
    return 0;
}

int solve(Grid grid, int numSteps) {
    int *distances = malloc(grid.width * grid.height * sizeof(int));
    memset(distances, -1, grid.width * grid.height * sizeof(int));
    Coord start = findStart(grid);
    bfs(grid, start, distances);

    int count = 0;
    for (int i = 0; i < grid.width * grid.height; i++) {
        if (distances[i] != -1 && distances[i] <= numSteps && distances[i] % 2 == 0) {
            count++;
        }
    }
    free(distances);
    return count;
}

char **readFile(const char *fileName, int *height) {
    FILE *file = fopen(fileName, "r");
    if (!file) exit(EXIT_FAILURE);

    char **lines = NULL;
    size_t size = 0;
    char buffer[256];
    while (fgets(buffer, sizeof(buffer), file)) {
        lines = realloc(lines, (size + 1) * sizeof(char *));
        lines[size] = strdup(buffer);
        size++;
    }
    fclose(file);
    *height = size;
    return lines;
}

void freeGrid(Grid grid) {
    for (int i = 0; i < grid.height; i++) {
        free(grid.data[i]);
    }
    free(grid.data);
}

int main() {
    int height;
    char **input = readFile("input.txt", &height);
    Grid grid = parseInput(input, height);
    printf("%d\n", solve(grid, 64));
    freeGrid(grid);
    for (int i = 0; i < height; i++) free(input[i]);
    free(input);
    return 0;
}