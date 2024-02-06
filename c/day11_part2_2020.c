
#include <stdio.h>

#define ROWS 100
#define COLS 100

typedef struct {
    int x, y;
} Point;

Point directions[] = {
    {-1, -1}, {0, -1}, {1, -1},
    {-1, 0}, /*{0,0},*/ {1, 0},
    {-1, 1}, {0, 1}, {1, 1},
};

int countVisibleOccupied(char seatingArea[ROWS][COLS], int row, int col) {
    int count = 0;
    for (int k = 0; k < 8; k++) {
        for (int r = row + directions[k].y, c = col + directions[k].x; r >= 0 && r < ROWS && c >= 0 && c < COLS;
             r += directions[k].y, c += directions[k].x) {
            if (seatingArea[r][c] == 'L') {
                break;
            }
            if (seatingArea[r][c] == '#') {
                count++;
                break;
            }
        }
    }
    return count;
}

int countOccupiedSeats(char seatingArea[ROWS][COLS]) {
    int count = 0;
    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            if (seatingArea[i][j] == '#') {
                count++;
            }
        }
    }
    return count;
}

int main() {
    FILE *file;
    file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char seatingArea[ROWS][COLS];
    for (int i = 0; i < ROWS; i++) {
        fscanf(file, "%s", seatingArea[i]);
    }
    fclose(file);

    int rows = ROWS;
    int cols = COLS;
    char newSeatingArea[ROWS][COLS];
    int stabilized = 0;

    while (!stabilized) {
        stabilized = 1;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                switch (seatingArea[i][j]) {
                    case 'L':
                        if (countVisibleOccupied(seatingArea, i, j) == 0) {
                            newSeatingArea[i][j] = '#';
                            stabilized = 0;
                        }
                        break;
                    case '#':
                        if (countVisibleOccupied(seatingArea, i, j) >= 5) {
                            newSeatingArea[i][j] = 'L';
                            stabilized = 0;
                        }
                        break;
                    default:
                        newSeatingArea[i][j] = seatingArea[i][j];
                }
            }
        }
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                seatingArea[i][j] = newSeatingArea[i][j];
            }
        }
    }

    printf("%d\n", countOccupiedSeats(seatingArea));

    return 0;
}
