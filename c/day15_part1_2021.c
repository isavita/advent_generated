
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX 1000 // Assuming the grid will not exceed 1000x1000

typedef struct {
    int x, y, risk;
} Position;

Position pq[MAX * MAX];
int pqSize = 0;

int compare(const void *a, const void *b) {
    Position *posA = (Position *)a;
    Position *posB = (Position *)b;
    return posA->risk - posB->risk;
}

void push(Position pos) {
    pq[pqSize++] = pos;
    qsort(pq, pqSize, sizeof(Position), compare);
}

Position pop() {
    Position top = pq[0];
    for (int i = 1; i < pqSize; i++) {
        pq[i - 1] = pq[i];
    }
    pqSize--;
    return top;
}

int dijkstra(int grid[MAX][MAX], int rows, int cols) {
    int dist[MAX][MAX];
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            dist[i][j] = INT_MAX;
        }
    }
    dist[0][0] = 0;

    push((Position){0, 0, 0});

    Position directions[] = {{1, 0, 0}, {0, 1, 0}, {-1, 0, 0}, {0, -1, 0}};
    while (pqSize > 0) {
        Position curr = pop();
        if (curr.x == rows - 1 && curr.y == cols - 1) {
            return curr.risk;
        }
        for (int i = 0; i < 4; i++) {
            int nx = curr.x + directions[i].x;
            int ny = curr.y + directions[i].y;
            if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
                int nextRisk = curr.risk + grid[nx][ny];
                if (nextRisk < dist[nx][ny]) {
                    dist[nx][ny] = nextRisk;
                    push((Position){nx, ny, nextRisk});
                }
            }
        }
    }
    return -1;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    int grid[MAX][MAX];
    int rows = 0, cols = 0;
    char line[MAX + 1];

    while (fgets(line, sizeof(line), file)) {
        cols = 0;
        for (int i = 0; line[i] != '\n' && line[i] != '\0'; i++) {
            grid[rows][cols++] = line[i] - '0';
        }
        rows++;
    }

    fclose(file);

    printf("%d\n", dijkstra(grid, rows, cols));

    return 0;
}
