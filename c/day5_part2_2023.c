
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct RangeMap {
    int srcStart, destStart, length;
} RangeMap;

int reverseConvertNumber(int number, RangeMap *ranges, int count) {
    for (int i = count - 1; i >= 0; i--) {
        RangeMap r = ranges[i];
        if (number >= r.destStart && number < r.destStart + r.length) {
            return r.srcStart + (number - r.destStart);
        }
    }
    return number;
}

int isInSeedRanges(int number, int(*ranges)[2], int count) {
    for (int i = 0; i < count; i++) {
        if (number >= ranges[i][0] && number < ranges[i][0] + ranges[i][1]) {
            return 1;
        }
    }
    return 0;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[255];
    int seedRanges[100][2];
    RangeMap currentRanges[100];
    RangeMap maps[100][100];
    int mapCount = 0;
    int currentRangeCount = 0;

    while (fgets(line, sizeof(line), file)) {
        if (strstr(line, "map:") != NULL) {
            if (currentRangeCount > 0) {
                memcpy(maps[mapCount], currentRanges, currentRangeCount * sizeof(RangeMap));
                mapCount++;
                currentRangeCount = 0;
            }
        } else if (strncmp(line, "seeds:", 6) == 0) {
            char *seedStrs = strtok(line + 7, " ");
            int i = 0;
            while (seedStrs != NULL) {
                int start = atoi(seedStrs);
                seedStrs = strtok(NULL, " ");
                int length = atoi(seedStrs);
                seedRanges[i][0] = start;
                seedRanges[i][1] = length;
                i++;
                seedStrs = strtok(NULL, " ");
            }
        } else {
            int numbers[3];
            int count = sscanf(line, "%d %d %d", &numbers[0], &numbers[1], &numbers[2]);
            if (count == 3) {
                currentRanges[currentRangeCount].srcStart = numbers[1];
                currentRanges[currentRangeCount].destStart = numbers[0];
                currentRanges[currentRangeCount].length = numbers[2];
                currentRangeCount++;
            }
        }
    }
    if (currentRangeCount > 0) {
        memcpy(maps[mapCount], currentRanges, currentRangeCount * sizeof(RangeMap));
        mapCount++;
    }

    // Finding the lowest location number
    for (int location = 0; ; location++) {
        int seed = location;
        for (int i = mapCount - 1; i >= 0; i--) {
            seed = reverseConvertNumber(seed, maps[i], currentRangeCount);
        }

        if (isInSeedRanges(seed, seedRanges, mapCount)) {
            printf("%d\n", location);
            break;
        }
    }

    fclose(file);
    return 0;
}
