
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define gridSize 1000

void processInstruction(char* instruction, bool grid[gridSize][gridSize]) {
    int startX, startY, endX, endY;
    if (sscanf(instruction, "turn on %d,%d through %d,%d", &startX, &startY, &endX, &endY) == 4 ||
        sscanf(instruction, "turn off %d,%d through %d,%d", &startX, &startY, &endX, &endY) == 4 ||
        sscanf(instruction, "toggle %d,%d through %d,%d", &startX, &startY, &endX, &endY) == 4) {
        for (int x = startX; x <= endX; x++) {
            for (int y = startY; y <= endY; y++) {
                if (strstr(instruction, "turn on") != NULL) {
                    grid[x][y] = true;
                } else if (strstr(instruction, "turn off") != NULL) {
                    grid[x][y] = false;
                } else if (strstr(instruction, "toggle") != NULL) {
                    grid[x][y] = !grid[x][y];
                }
            }
        }
    }
}

int countLights(bool grid[gridSize][gridSize]) {
    int count = 0;
    for (int i = 0; i < gridSize; i++) {
        for (int j = 0; j < gridSize; j++) {
            if (grid[i][j]) {
                count++;
            }
        }
    }
    return count;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    bool grid[gridSize][gridSize] = {false};
    char instruction[100];

    while (fgets(instruction, sizeof(instruction), file) != NULL) {
        processInstruction(instruction, grid);
    }

    fclose(file);

    printf("%d\n", countLights(grid));
    return 0;
}
