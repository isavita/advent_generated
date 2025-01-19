
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_ROWS 141
#define MAX_COLS 141

typedef struct {
    int x, y, dir;
} State;

bool loops(char grid[MAX_ROWS][MAX_COLS], int sx, int sy, int sdir, int h, int w) {
    int dirs[4][2] = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}};
    int x = sx, y = sy, dir = sdir;
    bool seen[MAX_ROWS * MAX_COLS * 4] = {false}; 

    for (int step = 0; step < 2000000; step++) {
        int state_index = (y * w + x) * 4 + dir;
        if (seen[state_index]) {
            return true;
        }
        seen[state_index] = true;

        int dx = dirs[dir][0];
        int dy = dirs[dir][1];
        int nx = x + dx;
        int ny = y + dy;

        if (nx < 0 || nx >= w || ny < 0 || ny >= h) {
            return false;
        }
        if (grid[ny][nx] == '#') {
            dir = (dir + 1) % 4;
            continue;
        }
        x = nx;
        y = ny;
    }
    return false;
}

int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening file");
        return 1;
    }

    char grid[MAX_ROWS][MAX_COLS];
    int h = 0;
    int w;
    char line[MAX_COLS + 2];

    while (fgets(line, sizeof(line), f)) {
        int len = strlen(line);
        if (line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }
        strcpy(grid[h], line);
        if(h==0)
            w=len;
        h++;

    }
    fclose(f);

    int startX, startY, startDir;
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            switch (grid[i][j]) {
                case '^':
                    startX = j;
                    startY = i;
                    startDir = 0;
                    break;
                case '>':
                    startX = j;
                    startY = i;
                    startDir = 1;
                    break;
                case 'v':
                    startX = j;
                    startY = i;
                    startDir = 2;
                    break;
                case '<':
                    startX = j;
                    startY = i;
                    startDir = 3;
                    break;
            }
        }
    }
    grid[startY][startX] = '.';

    int canLoop = 0;
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            if (x == startX && y == startY) {
                continue;
            }
            if (grid[y][x] != '.') {
                continue;
            }
            grid[y][x] = '#';
            if (loops(grid, startX, startY, startDir, h, w)) {
                canLoop++;
            }
             grid[y][x] = '.';
        }
    }

    printf("%d\n", canLoop);
    return 0;
}
