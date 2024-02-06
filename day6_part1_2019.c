
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ORBITS 2000
#define MAX_LINE_LENGTH 10

typedef struct {
    char key[4];
    char value[4];
} Orbit;

int countOrbits(Orbit orbits[], int size, char start[], int depth) {
    int count = depth;
    for (int i = 0; i < size; i++) {
        if (strcmp(orbits[i].key, start) == 0) {
            count += countOrbits(orbits, size, orbits[i].value, depth + 1);
        }
    }
    return count;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Orbit orbits[MAX_ORBITS];
    char line[MAX_LINE_LENGTH];
    int size = 0;

    while (fgets(line, sizeof(line), file)) {
        if (sscanf(line, "%3[^)])%3s", orbits[size].key, orbits[size].value) == 2) {
            size++;
        }
    }
    fclose(file);

    int totalOrbits = countOrbits(orbits, size, "COM", 0);
    printf("%d\n", totalOrbits);

    return 0;
}
