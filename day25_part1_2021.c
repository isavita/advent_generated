
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 1000

int moveEast(char grid[MAX_SIZE][MAX_SIZE], int height, int width);
int moveSouth(char grid[MAX_SIZE][MAX_SIZE], int height, int width);
void freeEmptyPositions(char grid[MAX_SIZE][MAX_SIZE], char oldPositions[MAX_SIZE][MAX_SIZE], int height, int width);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char grid[MAX_SIZE][MAX_SIZE];
    char line[MAX_SIZE];
    int height = 0, width = 0;

    while (fgets(line, sizeof(line), file)) {
        if (line[strlen(line) - 1] == '\n') {
            line[strlen(line) - 1] = '\0'; // Remove newline character
        }
        strcpy(grid[height], line);
        height++;
    }
    width = strlen(grid[0]);
    fclose(file);

    int step = 0;
    while (1) {
        int eastMoved = moveEast(grid, height, width);
        int southMoved = moveSouth(grid, height, width);
        step++;

        if (!eastMoved && !southMoved) {
            break;
        }
    }

    printf("%d\n", step);
    return EXIT_SUCCESS;
}

int moveEast(char grid[MAX_SIZE][MAX_SIZE], int height, int width) {
    int moved = 0;
    char oldPositions[MAX_SIZE][MAX_SIZE] = {0};

    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (grid[y][x] == '>') {
                int nextX = (x + 1) % width;
                if (grid[y][nextX] == '.') {
                    oldPositions[y][x] = '.';
                    grid[y][nextX] = '>';
                    x++; // Skip next position as it's now occupied
                    moved = 1;
                }
            }
        }
    }
    freeEmptyPositions(grid, oldPositions, height, width);

    return moved;
}

int moveSouth(char grid[MAX_SIZE][MAX_SIZE], int height, int width) {
    int moved = 0;
    char oldPositions[MAX_SIZE][MAX_SIZE] = {0};

    for (int x = 0; x < width; x++) {
        for (int y = 0; y < height; y++) {
            if (grid[y][x] == 'v') {
                int nextY = (y + 1) % height;
                if (grid[nextY][x] == '.') {
                    oldPositions[y][x] = '.';
                    grid[nextY][x] = 'v';
                    y++; // Skip next position as it's now occupied
                    moved = 1;
                }
            }
        }
    }
    freeEmptyPositions(grid, oldPositions, height, width);

    return moved;
}

void freeEmptyPositions(char grid[MAX_SIZE][MAX_SIZE], char oldPositions[MAX_SIZE][MAX_SIZE], int height, int width) {
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (oldPositions[y][x] == '.') {
                grid[y][x] = '.';
            }
        }
    }
}
