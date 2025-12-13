
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 1024

char grid[MAX][MAX];
int active[MAX], next[MAX];
int width, height;

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;
    while (fgets(grid[height], MAX, f)) {
        grid[height][strcspn(grid[height], "\n")] = 0;
        if (!width) width = strlen(grid[height]);
        height++;
    }
    fclose(f);

    int sx = 0, sy = 0;
    for (int y = 0; y < height; y++)
        for (int x = 0; x < width; x++)
            if (grid[y][x] == 'S') { sx = x; sy = y; goto found; }
found:

    int splits = 0;
    active[sx] = 1;

    for (int y = sy; y < height; y++) {
        memset(next, 0, sizeof(next));
        for (int x = 0; x < width; x++) {
            if (!active[x]) continue;
            if (grid[y][x] == '^') {
                splits++;
                if (x - 1 >= 0) next[x - 1] = 1;
                if (x + 1 < width) next[x + 1] = 1;
            } else {
                next[x] = 1;
            }
        }
        memcpy(active, next, sizeof(active));
        int any = 0;
        for (int i = 0; i < width; i++) if (active[i]) { any = 1; break; }
        if (!any) break;
    }

    printf("%d\n", splits);
    return 0;
}
