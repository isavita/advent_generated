
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ROBOTS 1000
#define MAX_LINE_LENGTH 100

typedef struct {
    int x, y;
    int vx, vy;
} Robot;

int mod(int a, int b) {
    return (a % b + b) % b;
}

Robot parseLine(const char *line) {
    Robot robot;
    sscanf(line, "p=%d,%d v=%d,%d", &robot.x, &robot.y, &robot.vx, &robot.vy);
    return robot;
}

void moveRobots(Robot *robots, int numRobots, int sizeX, int sizeY) {
    for (int i = 0; i < numRobots; i++) {
        robots[i].x = mod(robots[i].x + robots[i].vx, sizeX);
        robots[i].y = mod(robots[i].y + robots[i].vy, sizeY);
    }
}

int countQuadrants(Robot *robots, int numRobots, int sizeX, int sizeY) {
    int counts[4] = {0};
    int centerX = sizeX / 2;
    int centerY = sizeY / 2;

    for (int i = 0; i < numRobots; i++) {
        int x = robots[i].x;
        int y = robots[i].y;
        if (x < centerX) {
            if (y < centerY) counts[0]++;
            else if (y > centerY) counts[1]++;
        } else if (x > centerX) {
            if (y < centerY) counts[2]++;
            else if (y > centerY) counts[3]++;
        }
    }

    int safetyFactor = 1;
    for (int i = 0; i < 4; i++) {
        safetyFactor *= counts[i];
    }
    return safetyFactor;
}

int hasNoOverlaps(Robot *robots, int numRobots) {
    int positions[101][103] = {0};
    for (int i = 0; i < numRobots; i++) {
        if (positions[robots[i].x][robots[i].y]++) {
            return 0;
        }
    }
    return 1;
}

void drawGrid(Robot *robots, int numRobots, int sizeX, int sizeY) {
    char grid[103][101];
    memset(grid, '.', sizeof(grid));

    for (int i = 0; i < numRobots; i++) {
        grid[robots[i].y][robots[i].x] = '#';
    }

    for (int y = 0; y < sizeY; y++) {
        for (int x = 0; x < sizeX; x++) {
            putchar(grid[y][x]);
        }
        putchar('\n');
    }
}

int main() {
    int sizeX = 101;
    int sizeY = 103;
    Robot robots[MAX_ROBOTS];
    int numRobots = 0;

    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        if (line[0] != '\n') {
            robots[numRobots++] = parseLine(line);
        }
    }
    fclose(file);

    Robot robotsPart1[MAX_ROBOTS];
    memcpy(robotsPart1, robots, numRobots * sizeof(Robot));

    for (int n = 0; n < 100; n++) {
        moveRobots(robotsPart1, numRobots, sizeX, sizeY);
    }

    int safetyFactor = countQuadrants(robotsPart1, numRobots, sizeX, sizeY);
    printf("Part 1 - Safety Factor after 100 seconds: %d\n", safetyFactor);

    Robot robotsPart2[MAX_ROBOTS];
    memcpy(robotsPart2, robots, numRobots * sizeof(Robot));

    int seconds = 0;
    while (!hasNoOverlaps(robotsPart2, numRobots)) {
        moveRobots(robotsPart2, numRobots, sizeX, sizeY);
        seconds++;
        if (seconds > 1000000) {
            printf("Exceeded maximum iterations without finding a unique position configuration.\n");
            return 1;
        }
    }

    printf("Part 2 - Fewest seconds to display Easter egg: %d\n", seconds);
    printf("Final positions of robots:\n");
    drawGrid(robotsPart2, numRobots, sizeX, sizeY);

    return 0;
}
