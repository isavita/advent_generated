
#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 1000

int grid[MAX_SIZE][MAX_SIZE];
int visible[MAX_SIZE][MAX_SIZE];
int Neighbors4[4][2] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

void addPoint(int x, int y, int val) {
    grid[x][y] = val;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char c;
    int x = 0, y = 0;
    while ((c = fgetc(file)) != EOF) {
        if (c == '\n') {
            x = 0;
            y++;
        } else {
            addPoint(x, y, c - '0');
            x++;
        }
    }
    fclose(file);

    int size = y + 1;
    int i, j, k, l, nextX, nextY;

    for (i = 0; i < size; i++) {
        for (j = 0; j < size; j++) {
            for (k = 0; k < 4; k++) {
                nextX = i;
                nextY = j;
                while (1) {
                    nextX += Neighbors4[k][0];
                    nextY += Neighbors4[k][1];
                    if (nextX < 0 || nextX >= size || nextY < 0 || nextY >= size) {
                        visible[i][j] = 1;
                        break;
                    }
                    if (grid[nextX][nextY] >= grid[i][j]) {
                        break;
                    }
                }
            }
        }
    }

    int count = 0;
    for (i = 0; i < size; i++) {
        for (j = 0; j < size; j++) {
            if (visible[i][j] == 1) {
                count++;
            }
        }
    }

    printf("%d\n", count);

    return 0;
}
