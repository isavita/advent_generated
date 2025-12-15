
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_WIDTH 1000
#define MAX_HEIGHT 1000
#define MAX_LINE_LENGTH 2000

typedef struct {
    int key;
    uint64_t count;
} CountEntry;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char grid[MAX_HEIGHT][MAX_WIDTH];
    int height = 0;
    int width = 0;

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0;
        if (line[0] != '\0') {
            strcpy(grid[height], line);
            if (height == 0) {
                width = strlen(line);
            }
            height++;
        }
    }
    fclose(file);

    if (height == 0) {
        printf("0\n");
        return 0;
    }

    int startX = -1, startY = -1;
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (grid[y][x] == 'S') {
                startX = x;
                startY = y;
                break;
            }
        }
        if (startX != -1) break;
    }

    if (startX == -1) {
        fprintf(stderr, "Start point 'S' not found\n");
        return 1;
    }

    CountEntry counts[MAX_WIDTH * 2];
    int countSize = 0;
    counts[countSize].key = startX;
    counts[countSize].count = 1;
    countSize++;

    for (int y = startY; y < height; y++) {
        CountEntry nextCounts[MAX_WIDTH * 2];
        int nextCountSize = 0;

        for (int i = 0; i < countSize; i++) {
            int x = counts[i].key;
            uint64_t count = counts[i].count;

            int isSplitter = 0;
            if (x >= 0 && x < width && grid[y][x] == '^') {
                isSplitter = 1;
            }

            if (isSplitter) {
                int keys[] = {x - 1, x + 1};
                for (int k = 0; k < 2; k++) {
                    int key = keys[k];
                    int found = 0;
                    for (int j = 0; j < nextCountSize; j++) {
                        if (nextCounts[j].key == key) {
                            nextCounts[j].count += count;
                            found = 1;
                            break;
                        }
                    }
                    if (!found) {
                        nextCounts[nextCountSize].key = key;
                        nextCounts[nextCountSize].count = count;
                        nextCountSize++;
                    }
                }
            } else {
                int found = 0;
                for (int j = 0; j < nextCountSize; j++) {
                    if (nextCounts[j].key == x) {
                        nextCounts[j].count += count;
                        found = 1;
                        break;
                    }
                }
                if (!found) {
                    nextCounts[nextCountSize].key = x;
                    nextCounts[nextCountSize].count = count;
                    nextCountSize++;
                }
            }
        }

        countSize = nextCountSize;
        memcpy(counts, nextCounts, countSize * sizeof(CountEntry));
    }

    uint64_t total = 0;
    for (int i = 0; i < countSize; i++) {
        total += counts[i].count;
    }

    printf("%lu\n", total);
    return 0;
}
