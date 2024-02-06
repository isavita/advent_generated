
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int X, Y, Z;
} Coordinate;

Coordinate Zero = {0, 0, 0};

int abs(int a) {
    return a < 0 ? -a : a;
}

int distance(Coordinate c1, Coordinate c2) {
    return abs(c1.X - c2.X) + abs(c1.Y - c2.Y) + abs(c1.Z - c2.Z);
}

typedef struct {
    Coordinate coord;
    int radius;
} Bot;

typedef struct {
    Bot *bots;
    int size;
} Bots;

Bots newBots(char **input, int numLines) {
    Bots bots;
    bots.size = numLines;
    bots.bots = malloc(numLines * sizeof(Bot));

    for (int i = 0; i < numLines; i++) {
        sscanf(input[i], "pos=<%d,%d,%d>, r=%d", &bots.bots[i].coord.X, &bots.bots[i].coord.Y, &bots.bots[i].coord.Z, &bots.bots[i].radius);
    }

    return bots;
}

int haveInRange(Bots bots, Coordinate pos) {
    int sum = 0;

    for (int i = 0; i < bots.size; i++) {
        if (distance(bots.bots[i].coord, pos) <= bots.bots[i].radius) {
            sum++;
        }
    }

    return sum;
}

int strongestReachable(Bots bots) {
    int largestRadius = 0;
    int count = 0;
    Coordinate largestPos;

    for (int i = 0; i < bots.size; i++) {
        if (bots.bots[i].radius > largestRadius) {
            largestPos = bots.bots[i].coord;
            largestRadius = bots.bots[i].radius;
        }
    }

    for (int i = 0; i < bots.size; i++) {
        if (distance(largestPos, bots.bots[i].coord) <= largestRadius) {
            count++;
        }
    }

    return count;
}

int closestSuccess(Bots bots) {
    Coordinate cur, topLeft, bottomRight;
    int zoom = 1 << (sizeof(int) * 8 - 2);

    while (1) {
        Bots zoomedBots;
        zoomedBots.size = bots.size;
        zoomedBots.bots = malloc(bots.size * sizeof(Bot));

        for (int i = 0; i < bots.size; i++) {
            zoomedBots.bots[i].coord.X = bots.bots[i].coord.X / zoom;
            zoomedBots.bots[i].coord.Y = bots.bots[i].coord.Y / zoom;
            zoomedBots.bots[i].coord.Z = bots.bots[i].coord.Z / zoom;
            zoomedBots.bots[i].radius = bots.bots[i].radius / zoom;
        }

        Coordinate bestPos;
        int bestCount = 0;

        for (cur.X = topLeft.X; cur.X <= bottomRight.X; cur.X++) {
            for (cur.Y = topLeft.Y; cur.Y <= bottomRight.Y; cur.Y++) {
                for (cur.Z = topLeft.Z; cur.Z <= bottomRight.Z; cur.Z++) {
                    int c = haveInRange(zoomedBots, cur);

                    if (c < bestCount) {
                        continue;
                    }
                    if (c == bestCount && distance(Zero, cur) >= distance(Zero, bestPos)) {
                        continue;
                    }

                    bestPos = cur;
                    bestCount = c;
                }
            }
        }

        topLeft.X = (bestPos.X - 1) << 1;
        topLeft.Y = (bestPos.Y - 1) << 1;
        topLeft.Z = (bestPos.Z - 1) << 1;
        bottomRight.X = (bestPos.X + 1) << 1;
        bottomRight.Y = (bestPos.Y + 1) << 1;
        bottomRight.Z = (bestPos.Z + 1) << 1;
        zoom >>= 1;

        if (zoom == 0) {
            return distance(Zero, bestPos);
        }

        free(zoomedBots.bots);
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[100];
    char **lines = malloc(100 * sizeof(char*));
    int numLines = 0;

    while (fgets(line, sizeof(line), file)) {
        lines[numLines] = strdup(line);
        numLines++;
    }

    Bots bots = newBots(lines, numLines);
    printf("%d\n", closestSuccess(bots));

    for (int i = 0; i < numLines; i++) {
        free(lines[i]);
    }
    free(lines);
    free(bots.bots);

    fclose(file);

    return 0;
}
