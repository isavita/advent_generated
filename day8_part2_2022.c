
#include <stdio.h>
#include <stdlib.h>

struct Point {
    int x;
    int y;
};

struct Point Neighbors4[] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char c;
    int x = 0, y = 0;
    int grid[100][100] = {{0}};
    while ((c = fgetc(file)) != EOF) {
        if (c == '\n') {
            x = 0;
            y++;
        } else {
            grid[y][x] = c - '0';
            x++;
        }
    }

    int maxScore = 0;
    for (int i = 0; i < y; i++) {
        for (int j = 0; j < x; j++) {
            struct Point p = {j, i};
            int score = 1;
            for (int k = 0; k < 4; k++) {
                struct Point n = Neighbors4[k];
                struct Point next = p;
                int view = 0;
                while (1) {
                    next.x += n.x;
                    next.y += n.y;
                    if (next.x < 0 || next.x >= x || next.y < 0 || next.y >= y) {
                        score *= view;
                        break;
                    }
                    view++;
                    if (grid[next.y][next.x] >= grid[p.y][p.x]) {
                        score *= view;
                        break;
                    }
                }
            }
            if (score > maxScore) {
                maxScore = score;
            }
        }
    }
    printf("%d\n", maxScore);

    fclose(file);
    return 0;
}
