
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GRID_SIZE 101 // Since we are considering -50 to 50, inclusive
#define MIN_COORD -50
#define MAX_COORD 50

typedef struct {
    char action[4]; // "on" or "off"
    int xStart, xEnd;
    int yStart, yEnd;
    int zStart, zEnd;
} RebootStep;

int parseRebootStep(char* line, RebootStep* step) {
    char* token = strtok(line, " ");
    strcpy(step->action, token);

    token = strtok(NULL, ",");
    sscanf(token, "x=%d..%d", &step->xStart, &step->xEnd);

    token = strtok(NULL, ",");
    sscanf(token, "y=%d..%d", &step->yStart, &step->yEnd);

    token = strtok(NULL, ",");
    sscanf(token, "z=%d..%d", &step->zStart, &step->zEnd);

    return 0;
}

void executeRebootSteps(int cubeGrid[GRID_SIZE][GRID_SIZE][GRID_SIZE], RebootStep* steps, int stepsCount) {
    for (int i = 0; i < stepsCount; i++) {
        RebootStep step = steps[i];
        if (!(step.xStart >= MIN_COORD && step.xEnd <= MAX_COORD && step.yStart >= MIN_COORD && step.yEnd <= MAX_COORD && step.zStart >= MIN_COORD && step.zEnd <= MAX_COORD)) {
            continue;
        }
        for (int x = step.xStart; x <= step.xEnd; x++) {
            for (int y = step.yStart; y <= step.yEnd; y++) {
                for (int z = step.zStart; z <= step.zEnd; z++) {
                    cubeGrid[x - MIN_COORD][y - MIN_COORD][z - MIN_COORD] = strcmp(step.action, "on") == 0 ? 1 : 0;
                }
            }
        }
    }
}

int countOnCubes(int cubeGrid[GRID_SIZE][GRID_SIZE][GRID_SIZE]) {
    int count = 0;
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            for (int k = 0; k < GRID_SIZE; k++) {
                if (cubeGrid[i][j][k]) {
                    count++;
                }
            }
        }
    }
    return count;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    int cubeGrid[GRID_SIZE][GRID_SIZE][GRID_SIZE] = {{{0}}};
    RebootStep steps[1000]; // Assuming we have at most 1000 steps
    int stepsCount = 0;
    char line[256];

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n') continue;
        line[strcspn(line, "\n")] = 0; // Remove newline character
        parseRebootStep(line, &steps[stepsCount++]);
    }

    fclose(file);

    executeRebootSteps(cubeGrid, steps, stepsCount);
    int onCubes = countOnCubes(cubeGrid);

    printf("%d\n", onCubes);

    return 0;
}
