
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 1000

typedef struct {
    int x;
    int y;
} Coord;

typedef struct {
    int width;
    int height;
    char data[MAX_GRID_SIZE][MAX_GRID_SIZE];
} Grid;


Coord North = {0, -1};
Coord West = {-1, 0};
Coord South = {0, 1};
Coord East = {1, 0};


Grid buildGrid(char** input, int rows, int cols) {
    Grid grid;
    grid.width = cols;
    grid.height = rows;
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            grid.data[y][x] = input[y][x];
        }
    }
    return grid;
}


bool isInBounds(Coord coord, Grid grid) {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height;
}


void shiftSingleRock(Grid *grid, Coord coord, Coord dir) {
    if (grid->data[coord.y][coord.x] == 'O') {
        Coord current = coord;
        Coord before = {current.x + dir.x, current.y + dir.y};

        while (isInBounds(before, *grid) && grid->data[before.y][before.x] == '.') {
            grid->data[before.y][before.x] = 'O';
            grid->data[current.y][current.x] = '.';
            current = before;
            before.x += dir.x;
            before.y += dir.y;
        }
    }
}


void shiftRocks(Grid *grid, Coord dir) {
    if (dir.x == 0 && dir.y == -1 || dir.x == -1 && dir.y == 0) { 
        for (int x = 0; x < grid->width; x++) {
            for (int y = 0; y < grid->height; y++) {
                Coord coord = {x, y};
                shiftSingleRock(grid, coord, dir);
            }
        }
    } else {
         for (int x = grid->width - 1; x >= 0; x--) {
            for (int y = grid->height - 1; y >= 0; y--) {
                Coord coord = {x, y};
                shiftSingleRock(grid, coord, dir);
            }
        }
    }
}


int calculateLoad(Grid grid) {
    int load = 0;
    for (int x = 0; x < grid.width; x++) {
        for (int y = 0; y < grid.height; y++) {
            if (grid.data[y][x] == 'O') {
                load += grid.height - y;
            }
        }
    }
    return load;
}


int solve(char** input, int rows, int cols) {
    Grid grid = buildGrid(input, rows, cols);
    shiftRocks(&grid, North);
    return calculateLoad(grid);
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *lines[MAX_GRID_SIZE];
    char buffer[MAX_GRID_SIZE * 2];
    int rows = 0;
    int cols = 0;

    while (fgets(buffer, sizeof(buffer), file) != NULL) {
      
        size_t len = strlen(buffer);
        if (len > 0 && buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
            len--;
        }

        lines[rows] = (char*)malloc(len + 1);
        if (lines[rows] == NULL) {
            perror("Memory allocation failed");
            fclose(file);
            return 1;
        }
       
        strcpy(lines[rows], buffer);
        if(rows == 0){
            cols = len;
        }
        rows++;
    }

    fclose(file);

    int result = solve(lines, rows, cols);
    printf("%d\n", result);

    for(int i = 0; i < rows; i++){
        free(lines[i]);
    }
    return 0;
}
