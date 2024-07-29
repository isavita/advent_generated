#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) return 1;

    char **grid = NULL;
    size_t rows = 0, cols = 0, capacity = 1;
    grid = malloc(capacity * sizeof(char *));
    
    char line[1024];
    while (fgets(line, sizeof(line), file)) {
        if (rows >= capacity) {
            capacity *= 2;
            grid = realloc(grid, capacity * sizeof(char *));
            if (!grid) return 1; // Check for realloc failure
        }
        grid[rows] = strdup(line);
        if (!grid[rows]) return 1; // Check for strdup failure
        cols = strlen(line) > cols ? strlen(line) : cols;
        rows++;
    }
    fclose(file);

    int x = 0, y = 0;
    for (; x < cols; x++) if (grid[0][x] == '|') break;

    int dx = 0, dy = 1;
    char letters[1024];
    size_t letter_count = 0;

    while (1) {
        if (y < 0 || y >= rows || x < 0 || x >= strlen(grid[y])) break;

        char cell = grid[y][x];
        if (cell == ' ') break;

        if (cell >= 'A' && cell <= 'Z') letters[letter_count++] = cell;

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

    letters[letter_count] = '\0';
    printf("%s\n", letters);

    for (size_t i = 0; i < rows; i++) free(grid[i]);
    free(grid);
    return 0;
}