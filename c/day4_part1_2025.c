
#include <stdio.h>
#include <stdlib.h>

#define MAX 1024

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;

    static char grid[MAX][MAX];
    int rows = 0, cols = 0;

    while (fgets(grid[rows], MAX, f)) {
        if (grid[rows][0] == '\n') continue;
        int len = 0;
        while (grid[rows][len] && grid[rows][len] != '\n') len++;
        grid[rows][len] = '\0';
        if (!cols) cols = len;
        rows++;
    }
    fclose(f);

    int dx[8] = {-1,-1,-1, 0, 0, 1, 1, 1};
    int dy[8] = {-1, 0, 1,-1, 1,-1, 0, 1};

    int acc = 0;
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            if (grid[y][x] != '@') continue;
            int cnt = 0;
            for (int d = 0; d < 8; d++) {
                int nx = x + dx[d], ny = y + dy[d];
                if (nx >= 0 && nx < cols && ny >= 0 && ny < rows && grid[ny][nx] == '@') cnt++;
            }
            if (cnt < 4) acc++;
        }
    }
    printf("%d\n", acc);
    return 0;
}
