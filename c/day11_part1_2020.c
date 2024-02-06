
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OCCUPIED '#'
#define EMPTY 'L'
#define FLOOR '.'

char** readInput(char* filename, int* rows, int* cols) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file.\n");
        exit(1);
    }

    char buffer[1024];
    int row = 0;
    char** seatLayout = NULL;

    while (fgets(buffer, sizeof(buffer), file) != NULL) {
        (*rows)++;
        if (seatLayout == NULL) {
            *cols = strlen(buffer) - 1;
            seatLayout = (char**)malloc((*rows) * sizeof(char*));
        } else {
            seatLayout = (char**)realloc(seatLayout, (*rows) * sizeof(char*));
        }
        seatLayout[row] = (char*)malloc(*cols * sizeof(char));
        strncpy(seatLayout[row], buffer, *cols);
        row++;
    }

    fclose(file);
    return seatLayout;
}

int countOccupiedAdjacent(char** seatLayout, int row, int col, int rows, int cols) {
    int count = 0;

    for (int i = row - 1; i <= row + 1; i++) {
        for (int j = col - 1; j <= col + 1; j++) {
            if (i >= 0 && i < rows && j >= 0 && j < cols && !(i == row && j == col)) {
                if (seatLayout[i][j] == OCCUPIED) {
                    count++;
                }
            }
        }
    }

    return count;
}

void simulateSeating(char** seatLayout, int rows, int cols) {
    char** newLayout = (char**)malloc(rows * sizeof(char*));

    for (int i = 0; i < rows; i++) {
        newLayout[i] = (char*)malloc(cols * sizeof(char));
        strcpy(newLayout[i], seatLayout[i]);
    }

    int changes = 1;
    while (changes > 0) {
        changes = 0;

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (seatLayout[i][j] == EMPTY && countOccupiedAdjacent(seatLayout, i, j, rows, cols) == 0) {
                    newLayout[i][j] = OCCUPIED;
                    changes++;
                } else if (seatLayout[i][j] == OCCUPIED && countOccupiedAdjacent(seatLayout, i, j, rows, cols) >= 4) {
                    newLayout[i][j] = EMPTY;
                    changes++;
                }
            }
        }

        for (int i = 0; i < rows; i++) {
            strcpy(seatLayout[i], newLayout[i]);
        }
    }

    for (int i = 0; i < rows; i++) {
        free(newLayout[i]);
    }
    free(newLayout);
}

int countOccupiedSeats(char** seatLayout, int rows, int cols) {
    int count = 0;
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (seatLayout[i][j] == OCCUPIED) {
                count++;
            }
        }
    }
    return count;
}

int main() {
    int rows = 0;
    int cols = 0;
    char** seatLayout = readInput("input.txt", &rows, &cols);

    simulateSeating(seatLayout, rows, cols);

    int occupiedSeats = countOccupiedSeats(seatLayout, rows, cols);
    printf("%d\n", occupiedSeats);

    for (int i = 0; i < rows; i++) {
        free(seatLayout[i]);
    }
    free(seatLayout);

    return 0;
}
