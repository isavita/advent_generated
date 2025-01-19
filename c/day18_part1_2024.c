
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define SIZE 71
#define MAX_QUEUE_SIZE SIZE * SIZE

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point pt;
    int steps;
} QueueItem;

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    bool grid[SIZE][SIZE] = {false};
    char line[16];
    int x, y;

    for (int i = 0; i < 1024 && fgets(line, sizeof(line), fp); i++) {
        if (sscanf(line, "%d,%d", &x, &y) == 2) {
            if (x >= 0 && x < SIZE && y >= 0 && y < SIZE) {
                grid[y][x] = true;
            }
        }
    }
    fclose(fp);
    
    int dx[] = {1, -1, 0, 0};
    int dy[] = {0, 0, 1, -1};
    bool visited[SIZE][SIZE] = {false};
    QueueItem queue[MAX_QUEUE_SIZE];
    int head = 0, tail = 0;

    queue[tail++] = (QueueItem){{0, 0}, 0};
    visited[0][0] = true;

    while (head < tail) {
        QueueItem current = queue[head++];
        if (current.pt.x == SIZE - 1 && current.pt.y == SIZE - 1) {
            printf("%d\n", current.steps);
            return 0;
        }

        for (int i = 0; i < 4; i++) {
            int nx = current.pt.x + dx[i];
            int ny = current.pt.y + dy[i];

            if (nx >= 0 && ny >= 0 && nx < SIZE && ny < SIZE && !grid[ny][nx] && !visited[ny][nx]) {
                visited[ny][nx] = true;
                queue[tail++] = (QueueItem){{nx, ny}, current.steps + 1};
            }
        }
    }
    printf("No path\n");
    return 0;
}
