
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 1024

typedef struct {
    int x, y;
} position;

int isInfected(position pos, position *infected, int count) {
    for (int i = 0; i < count; i++) {
        if (infected[i].x == pos.x && infected[i].y == pos.y) {
            return i; // Return index if found
        }
    }
    return -1; // Not found
}

void removeInfected(position *infected, int index, int *count) {
    for (int i = index; i < *count - 1; i++) {
        infected[i] = infected[i + 1];
    }
    (*count)--;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Could not open input file.\n");
        return 1;
    }

    char line[MAX_SIZE];
    position infected[MAX_SIZE];
    int infectedCount = 0;
    int y = 0;
    int startX = 0, startY = 0;

    while (fgets(line, MAX_SIZE, file) != NULL) {
        int len = strlen(line);
        if (line[len - 1] == '\n') line[len - 1] = '\0'; // Remove newline
        len--; // Update length after removing newline
        for (int x = 0; x < len; x++) {
            if (line[x] == '#') {
                infected[infectedCount++] = (position){x, y};
            }
        }
        startX = len / 2;
        y++;
    }
    startY = y / 2;
    fclose(file);

    int dx[] = {0, 1, 0, -1};
    int dy[] = {-1, 0, 1, 0};
    int dir = 0; // Start facing up
    int x = startX, newY = startY;
    int newInfections = 0;

    for (int i = 0; i < 10000; i++) {
        position currentPos = {x, newY};
        int index = isInfected(currentPos, infected, infectedCount);
        if (index != -1) { // Infected
            dir = (dir + 1) % 4; // Turn right
            removeInfected(infected, index, &infectedCount); // Clean
        } else { // Clean
            dir = (dir - 1 + 4) % 4; // Turn left
            infected[infectedCount++] = currentPos; // Infect
            newInfections++;
        }
        x += dx[dir];
        newY += dy[dir];
    }

    printf("%d\n", newInfections);
    return 0;
}
