#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) return 1;

    char **grid = NULL;
    size_t rows = 0, cols = 0, capacity = 10;
    grid = malloc(capacity * sizeof(char *));
    
    char line[1024];
    while (fgets(line, sizeof(line), file)) {
        if (rows >= capacity) {
            capacity *= 2;
            grid = realloc(grid, capacity * sizeof(char *));
        }
        grid[rows] = malloc((strlen(line) + 1) * sizeof(char));
        strcpy(grid[rows], line);
        if (cols < strlen(line)) cols = strlen(line);
        rows++;
    }
    fclose(file);

    int x = 0, y = 0, dx = 0, dy = 1, steps = 0;

    for (int i = 0; i < cols; i++) {
        if (grid[0][i] == '|') {
            x = i;
            break;
        }
    }

    while (1) {
        if (y < 0 || y >= rows || x < 0 || x >= strlen(grid[y])) break;
        char cell = grid[y][x];
        if (cell == ' ') break;

        steps++;
        if (cell == '+') {
            if (dx == 0) {
                dx = (x > 0 && (grid[y][x-1] == '-' || (grid[y][x-1] >= 'A' && grid[y][x-1] <= 'Z'))) ? -1 : 1;
                dy = 0;
            } else {
                dx = 0;
                dy = (y > 0 && (grid[y-1][x] == '|' || (grid[y-1][x] >= 'A' && grid[y-1][x] <= 'Z'))) ? -1 : 1;
            }
        }

        x += dx;
        y += dy;
    }

    printf("%d\n", steps);

    for (size_t i = 0; i < rows; i++) free(grid[i]);
    free(grid);
    return 0;
}