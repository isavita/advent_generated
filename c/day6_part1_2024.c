
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 1000

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char grid[MAX_GRID_SIZE][MAX_GRID_SIZE];
    int h = 0;
    int w = 0;
    char line[MAX_GRID_SIZE];

    while (fgets(line, sizeof(line), fp)) {
        int len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }
        strcpy(grid[h], line);
        if (h == 0)
            w = len;
        h++;
    }
    fclose(fp);

    int x, y, dirX, dirY, dirIdx;
    int dirs[4][2] = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}};
    bool found = false;

    for (int i = 0; i < h && !found; i++) {
        for (int j = 0; j < w && !found; j++) {
            switch (grid[i][j]) {
                case '^': x = j; y = i; dirIdx = 0; found = true; break;
                case '>': x = j; y = i; dirIdx = 1; found = true; break;
                case 'v': x = j; y = i; dirIdx = 2; found = true; break;
                case '<': x = j; y = i; dirIdx = 3; found = true; break;
            }
        }
    }
    dirX = dirs[dirIdx][0];
    dirY = dirs[dirIdx][1];


    bool visited[MAX_GRID_SIZE][MAX_GRID_SIZE] = {false};
    int count = 0;
    visited[y][x] = true;
    count++;


    while (true) {
        int nx = x + dirX;
        int ny = y + dirY;

        if (nx < 0 || nx >= w || ny < 0 || ny >= h)
            break;

        if (grid[ny][nx] == '#') {
            dirIdx = (dirIdx + 1) % 4;
            dirX = dirs[dirIdx][0];
            dirY = dirs[dirIdx][1];
            continue;
        }
        x = nx;
        y = ny;
        if (!visited[y][x]){
            visited[y][x] = true;
            count++;
        }

    }


    printf("%d\n", count);

    return 0;
}
