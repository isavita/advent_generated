
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

typedef struct {
    long long srcStart;
    long long destStart;
    long long length;
} RangeMap;

long long convertNumber(long long number, RangeMap *ranges, int rangeCount) {
    for (int i = 0; i < rangeCount; i++) {
        if (number >= ranges[i].srcStart && number < ranges[i].srcStart + ranges[i].length) {
            return ranges[i].destStart + (number - ranges[i].srcStart);
        }
    }
    return number;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    long long seeds[100];
    int seedCount = 0;
    RangeMap maps[10][100];
    int mapCounts[10] = {0};
    int mapIndex = -1;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    while ((read = getline(&line, &len, file)) != -1) {
        if (strstr(line, "map:") != NULL) {
            mapIndex++;
            mapCounts[mapIndex] = 0;
        } else if (strncmp(line, "seeds:", 6) == 0) {
             char *token = strtok(line + 7, " ");
                while (token != NULL) {
                    seeds[seedCount++] = atoll(token);
                    token = strtok(NULL, " ");
                }

        } else {
           long long destStart, srcStart, length;
            if (sscanf(line, "%lld %lld %lld", &destStart, &srcStart, &length) == 3) {
                maps[mapIndex][mapCounts[mapIndex]].srcStart = srcStart;
                maps[mapIndex][mapCounts[mapIndex]].destStart = destStart;
                maps[mapIndex][mapCounts[mapIndex]].length = length;
                mapCounts[mapIndex]++;
            }
        }
    }

    long long minLocation = LLONG_MAX;
    for (int i = 0; i < seedCount; i++) {
        long long location = seeds[i];
        for(int j = 0; j <= mapIndex; j++){
            location = convertNumber(location, maps[j], mapCounts[j]);
        }
        if (location < minLocation) {
            minLocation = location;
        }
    }

    printf("%lld\n", minLocation);

    if (line)
        free(line);
    fclose(file);
    return 0;
}
