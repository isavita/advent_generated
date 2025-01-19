
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    int dx, dy;
} Direction;

Direction getAllDirections[] = {
    {0, 1}, {1, 0}, {1, 1}, {-1, 1}, {0, -1}, {-1, 0}, {-1, -1}, {1, -1}
};

bool checkWord(char **grid, int rows, int cols, const char *word, int x, int y, Direction d) {
    int len = strlen(word);
    if (x < 0 || y < 0 || x >= rows || y >= cols) return false;

    for (int i = 0; i < len; i++) {
        int newX = x + (d.dx * i);
        int newY = y + (d.dy * i);
        if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || grid[newX][newY] != word[i]) {
             return false;
        }
    }
    return true;
}

int countOccurrences(char **grid, int rows, int cols, const char *word) {
    int count = 0;
    int numDirections = sizeof(getAllDirections) / sizeof(getAllDirections[0]);

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            for (int k = 0; k < numDirections; k++) {
                if (checkWord(grid, rows, cols, word, i, j, getAllDirections[k])) {
                    count++;
                }
            }
        }
    }
    return count;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    char **grid = NULL;
    int rows = 0;
    int cols = 0;

    while ((read = getline(&line, &len, file)) != -1) {
        if (line[read - 1] == '\n') {
            line[read - 1] = '\0';
            read--;
        }
        if (read > 0){
          grid = realloc(grid, (rows + 1) * sizeof(char*));
          grid[rows] = strdup(line);
          if (rows==0) cols = read;
          rows++;
        }

    }

    fclose(file);
    if(line) free(line);


    int count = countOccurrences(grid, rows, cols, "XMAS");
    printf("XMAS appears %d times in the word search\n", count);


    for (int i = 0; i < rows; i++) {
        free(grid[i]);
    }
    free(grid);


    return 0;
}
