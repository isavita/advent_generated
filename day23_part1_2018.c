
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int x, y, z, radius;
} Nanobot;

Nanobot parseNanobots(FILE *file);
Nanobot findStrongestNanobot(Nanobot nanobots[], int size);
int countNanobotsInRange(Nanobot nanobots[], Nanobot strongest, int size);
int manhattanDistance(Nanobot a, Nanobot b);
int abs(int x);

int main() {
    FILE *file = fopen("input.txt", "r");
    Nanobot nanobots[1000];
    int size = 0;

    while (!feof(file)) {
        nanobots[size] = parseNanobots(file);
        size++;
    }

    Nanobot strongest = findStrongestNanobot(nanobots, size);
    int inRangeCount = countNanobotsInRange(nanobots, strongest, size);

    printf("%d\n", inRangeCount);

    fclose(file);
    return 0;
}

Nanobot parseNanobots(FILE *file) {
    Nanobot nanobot;
    fscanf(file, "pos=<%d,%d,%d>, r=%d\n", &nanobot.x, &nanobot.y, &nanobot.z, &nanobot.radius);
    return nanobot;
}

Nanobot findStrongestNanobot(Nanobot nanobots[], int size) {
    Nanobot strongest = nanobots[0];
    for (int i = 1; i < size; i++) {
        if (nanobots[i].radius > strongest.radius) {
            strongest = nanobots[i];
        }
    }
    return strongest;
}

int countNanobotsInRange(Nanobot nanobots[], Nanobot strongest, int size) {
    int count = 0;
    for (int i = 0; i < size; i++) {
        if (manhattanDistance(nanobots[i], strongest) <= strongest.radius) {
            count++;
        }
    }
    return count;
}

int manhattanDistance(Nanobot a, Nanobot b) {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z);
}

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}
