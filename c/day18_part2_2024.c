
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define SIZE 71

bool canReach(bool grid[SIZE][SIZE]) {
    if (grid[0][0] || grid[SIZE - 1][SIZE - 1]) {
        return false;
    }

    int dirs[4][2] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
    bool visited[SIZE][SIZE];
    memset(visited, false, sizeof(visited));

    typedef struct { int x, y; } Point;
    Point queue[SIZE * SIZE];
    int head = 0, tail = 0;

    queue[tail++] = (Point){0, 0};
    visited[0][0] = true;

    while (head < tail) {
        Point current = queue[head++];
        if (current.x == SIZE - 1 && current.y == SIZE - 1) {
            return true;
        }
        for (int i = 0; i < 4; i++) {
            int nx = current.x + dirs[i][0];
            int ny = current.y + dirs[i][1];
            if (nx >= 0 && ny >= 0 && nx < SIZE && ny < SIZE && !grid[ny][nx] && !visited[ny][nx]) {
                visited[ny][nx] = true;
                queue[tail++] = (Point){nx, ny};
            }
        }
    }
    return false;
}

int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Failed to open input.txt");
        return 1;
    }

    bool grid[SIZE][SIZE];
    memset(grid, false, sizeof(grid));

    char line[256];
    int x, y;
    
    while (fgets(line, sizeof(line), f)) {
        if(sscanf(line, "%d,%d", &x, &y) == 2){
            if (x >= 0 && x < SIZE && y >= 0 && y < SIZE) {
                grid[y][x] = true;
            }

            if (!canReach(grid)) {
                printf("%d,%d\n", x, y);
                fclose(f);
                return 0;
            }
        }
    }

    printf("No cutoff found\n");
    fclose(f);
    return 0;
}
