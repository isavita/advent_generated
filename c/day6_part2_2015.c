
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GRID_SIZE 1000

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    int (*grid)[GRID_SIZE] = malloc(sizeof(int[GRID_SIZE][GRID_SIZE]));
    memset(grid, 0, sizeof(int[GRID_SIZE][GRID_SIZE]));

    char line[100];
    while (fgets(line, sizeof(line), file) != NULL) {
        int startX, startY, endX, endY;
        char *token = strtok(line, " ");
        char action[10];
        if (strcmp(token, "turn") == 0) {
            token = strtok(NULL, " ");
            strcpy(action, token);
        } else {
            strcpy(action, "toggle");
        }

        while (token != NULL) {
            if (strstr(token, ",") != NULL) {
               if(startX == 0 && startY == 0)
                sscanf(token, "%d,%d", &startX, &startY);
               else
                sscanf(token, "%d,%d", &endX, &endY);
            }
            token = strtok(NULL, " ");
        }

        for (int x = startX; x <= endX; x++) {
            for (int y = startY; y <= endY; y++) {
                if (strcmp(action, "on") == 0) {
                    grid[x][y]++;
                } else if (strcmp(action, "off") == 0) {
                    if (grid[x][y] > 0) {
                        grid[x][y]--;
                    }
                } else {
                    grid[x][y] += 2;
                }
            }
        }
        startX = 0;
        startY = 0;
    }

    fclose(file);

    long long brightness = 0;
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            brightness += grid[i][j];
        }
    }

    printf("%lld\n", brightness);
    free(grid);

    return 0;
}
