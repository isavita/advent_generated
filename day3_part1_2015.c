
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char directions[10000];
    fscanf(file, "%s", directions);
    fclose(file);

    int visitedHouses[1000][1000] = {0};
    int x = 500, y = 500; // Santa's starting position
    visitedHouses[x][y] = 1;

    for (int i = 0; directions[i] != '\0'; i++) {
        switch (directions[i]) {
            case '^':
                y++; // Move north
                break;
            case 'v':
                y--; // Move south
                break;
            case '>':
                x++; // Move east
                break;
            case '<':
                x--; // Move west
                break;
        }
        visitedHouses[x][y] = 1;
    }

    int count = 0;
    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < 1000; j++) {
            if (visitedHouses[i][j] == 1) {
                count++;
            }
        }
    }

    printf("%d\n", count);

    return 0;
}
