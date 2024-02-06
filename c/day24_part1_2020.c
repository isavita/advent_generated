
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int q, r;
} Coordinate;

Coordinate directions[6] = {{1, 0}, {0, 1}, {-1, 1}, {-1, 0}, {0, -1}, {1, -1}};

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    Coordinate blackTiles[1000];
    int blackCount[1000] = {0};
    int tilesCount = 0;

    char line[256];
    while (fgets(line, sizeof(line), file)) {
        Coordinate coord = {0, 0};
        for (int i = 0; i < strlen(line); i++) {
            int index = -1;
            if (line[i] == 'e') index = 0;
            else if (line[i] == 'w') index = 3;
            else if (line[i] == 'n' || line[i] == 's') {
                char dir[3] = {line[i], line[++i], '\0'};
                if (strcmp(dir, "se") == 0) index = 1;
                else if (strcmp(dir, "sw") == 0) index = 2;
                else if (strcmp(dir, "nw") == 0) index = 4;
                else if (strcmp(dir, "ne") == 0) index = 5;
            }
            if (index != -1) {
                coord.q += directions[index].q;
                coord.r += directions[index].r;
            }
        }

        int found = 0;
        for (int i = 0; i < tilesCount; i++) {
            if (blackTiles[i].q == coord.q && blackTiles[i].r == coord.r) {
                blackCount[i] = !blackCount[i];
                found = 1;
                break;
            }
        }
        if (!found) {
            blackTiles[tilesCount] = coord;
            blackCount[tilesCount++] = 1;
        }
    }
    fclose(file);

    int count = 0;
    for (int i = 0; i < tilesCount; i++) {
        if (blackCount[i]) count++;
    }
    printf("%d\n", count);

    return EXIT_SUCCESS;
}
