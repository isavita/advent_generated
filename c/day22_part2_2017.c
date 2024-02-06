
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 10000

enum { Clean, Weakened, Infected, Flagged };

typedef struct {
    int x, y;
} position;

int hash(position pos) {
    return (pos.x + MAX_SIZE) * MAX_SIZE + (pos.y + MAX_SIZE);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    int *grid = calloc(MAX_SIZE * MAX_SIZE * 4, sizeof(int)); // Dynamic allocation for a large grid
    if (!grid) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    char line[MAX_SIZE];
    int startX = 0, startY = 0, maxX = 0, maxY = 0;
    for (int y = 0; fgets(line, sizeof(line), file); y++) {
        size_t len = strlen(line);
        if (line[len - 1] == '\n') {
            line[len - 1] = '\0';
            --len;
        }
        for (int x = 0; x < len; x++) {
            if (line[x] == '#') {
                position pos = {x, y};
                grid[hash(pos)] = Infected;
            }
        }
        startX = len / 2;
        maxY = y;
    }
    startY = maxY / 2;
    fclose(file);

    int dx[] = {0, 1, 0, -1};
    int dy[] = {-1, 0, 1, 0};
    int x = startX, y = startY, dir = 0, infectedCount = 0;

    for (int i = 0; i < 10000000; i++) {
        position pos = {x, y};
        switch (grid[hash(pos)]) {
            case Clean:
                dir = (dir - 1 + 4) % 4;
                grid[hash(pos)] = Weakened;
                break;
            case Weakened:
                grid[hash(pos)] = Infected;
                infectedCount++;
                break;
            case Infected:
                dir = (dir + 1) % 4;
                grid[hash(pos)] = Flagged;
                break;
            case Flagged:
                dir = (dir + 2) % 4;
                grid[hash(pos)] = Clean;
                break;
        }
        x += dx[dir];
        y += dy[dir];
    }

    printf("%d\n", infectedCount);
    free(grid);
    return EXIT_SUCCESS;
}
