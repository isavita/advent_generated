
#include <stdio.h>

#define OPEN '.'
#define TREES '|'
#define LUMBERYARD '#'
#define SIZE 50

char nextAcreState(char grid[SIZE][SIZE], int i, int j);
int countAdjacent(char grid[SIZE][SIZE], int i, int j, char acreType);
void transform(char grid[SIZE][SIZE]);
void countResources(char grid[SIZE][SIZE], int *wooded, int *lumberyards);

int main() {
    char grid[SIZE][SIZE];
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error reading file\n");
        return 1;
    }

    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            fscanf(file, " %c", &grid[i][j]);
        }
    }
    fclose(file);

    for (int minute = 0; minute < 10; minute++) {
        transform(grid);
    }

    int wooded, lumberyards;
    countResources(grid, &wooded, &lumberyards);
    printf("%d\n", wooded * lumberyards);

    return 0;
}

char nextAcreState(char grid[SIZE][SIZE], int i, int j) {
    switch (grid[i][j]) {
        case OPEN:
            if (countAdjacent(grid, i, j, TREES) >= 3) {
                return TREES;
            }
            break;
        case TREES:
            if (countAdjacent(grid, i, j, LUMBERYARD) >= 3) {
                return LUMBERYARD;
            }
            break;
        case LUMBERYARD:
            if (countAdjacent(grid, i, j, LUMBERYARD) >= 1 && countAdjacent(grid, i, j, TREES) >= 1) {
                return LUMBERYARD;
            }
            return OPEN;
    }
    return grid[i][j];
}

int countAdjacent(char grid[SIZE][SIZE], int i, int j, char acreType) {
    int count = 0;
    for (int x = -1; x <= 1; x++) {
        for (int y = -1; y <= 1; y++) {
            if (x == 0 && y == 0) {
                continue;
            }
            if (i+x >= 0 && i+x < SIZE && j+y >= 0 && j+y < SIZE && grid[i+x][j+y] == acreType) {
                count++;
            }
        }
    }
    return count;
}

void transform(char grid[SIZE][SIZE]) {
    char newGrid[SIZE][SIZE];
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            newGrid[i][j] = nextAcreState(grid, i, j);
        }
    }
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            grid[i][j] = newGrid[i][j];
        }
    }
}

void countResources(char grid[SIZE][SIZE], int *wooded, int *lumberyards) {
    *wooded = 0;
    *lumberyards = 0;
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            switch (grid[i][j]) {
                case TREES:
                    (*wooded)++;
                    break;
                case LUMBERYARD:
                    (*lumberyards)++;
                    break;
            }
        }
    }
}
