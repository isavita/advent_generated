#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define WIDTH 40
#define HEIGHT 6

typedef struct {
    int x, y;
} Point;

int abs(int x) {
    return x < 0 ? -x : x;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    int x[256] = {1};
    int len = 1;
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        if (strcmp(line, "noop\n") == 0) {
            x[len++] = x[len-1];
        } else {
            int n;
            sscanf(line, "addx %d\n", &n);
            x[len++] = x[len-1];
            x[len++] = x[len-1] + n;
        }
    }
    fclose(file);

    Point grid[WIDTH*HEIGHT];
    int grid_len = 0;
    for (int i = 0; i < len; i++) {
        int crtx = i % WIDTH;
        int crty = i / WIDTH;
        if (abs(crtx - x[i]) <= 1) {
            grid[grid_len].x = crtx;
            grid[grid_len].y = crty;
            grid_len++;
        }
    }

    for (int y = 0; y < HEIGHT; y++) {
        for (int x = 0; x < WIDTH; x++) {
            int found = 0;
            for (int i = 0; i < grid_len; i++) {
                if (grid[i].x == x && grid[i].y == y) {
                    found = 1;
                    break;
                }
            }
            printf(found ? "#" : ".");
        }
        printf("\n");
    }

    return 0;
}