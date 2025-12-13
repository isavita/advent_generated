
#include <stdio.h>
#include <stdlib.h>

#define MAX 1024
char grid[MAX][MAX];
int R, C;

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 0;

    R = 0;
    while (fgets(grid[R], MAX, f)) {
        int len = 0;
        while (grid[R][len] && grid[R][len] != '\n') ++len;
        grid[R][len] = 0;
        if (len > C) C = len;
        ++R;
    }
    fclose(f);
    if (R == 0 || C == 0) {
        printf("Total rolls removed: 0\n");
        return 0;
    }

    int removed = 0, changed;
    const int dr[] = {-1,-1,-1, 0, 0, 1, 1, 1};
    const int dc[] = {-1, 0, 1,-1, 1,-1, 0, 1};

    do {
        changed = 0;
        for (int r = 0; r < R; ++r) {
            for (int c = 0; c < C; ++c) {
                if (grid[r][c] != '@') continue;
                int cnt = 0;
                for (int k = 0; k < 8; ++k) {
                    int nr = r + dr[k], nc = c + dc[k];
                    if (nr >= 0 && nr < R && nc >= 0 && nc < C && grid[nr][nc] == '@') ++cnt;
                }
                if (cnt < 4) grid[r][c] = '*', ++changed;
            }
        }
        for (int r = 0; r < R; ++r)
            for (int c = 0; c < C; ++c)
                if (grid[r][c] == '*') grid[r][c] = '.', ++removed;
    } while (changed);

    printf("Total rolls removed: %d\n", removed);
    return 0;
}
