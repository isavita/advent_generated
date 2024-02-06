
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define ROWS 100 // Adjust based on input size
#define COLS 100 // Adjust based on input size
#define EXTENDED_SIZE 5

typedef struct {
    int x, y, risk;
} Position;

typedef struct {
    Position* elements;
    int size;
    int capacity;
} PriorityQueue;

void swap(Position* a, Position* b) {
    Position temp = *a;
    *a = *b;
    *b = temp;
}

void pqPush(PriorityQueue* pq, Position value) {
    if (pq->size == pq->capacity) {
        pq->capacity *= 2;
        pq->elements = (Position*)realloc(pq->elements, pq->capacity * sizeof(Position));
    }
    pq->elements[pq->size] = value;
    int i = pq->size;
    while (i != 0 && pq->elements[(i - 1) / 2].risk > pq->elements[i].risk) {
        swap(&pq->elements[i], &pq->elements[(i - 1) / 2]);
        i = (i - 1) / 2;
    }
    pq->size++;
}

Position pqPop(PriorityQueue* pq) {
    Position root = pq->elements[0];
    pq->elements[0] = pq->elements[pq->size - 1];
    pq->size--;
    int i = 0;
    do {
        int minIndex = i;
        int left = 2 * i + 1;
        int right = 2 * i + 2;
        if (left < pq->size && pq->elements[left].risk < pq->elements[minIndex].risk) {
            minIndex = left;
        }
        if (right < pq->size && pq->elements[right].risk < pq->elements[minIndex].risk) {
            minIndex = right;
        }
        if (minIndex != i) {
            swap(&pq->elements[i], &pq->elements[minIndex]);
            i = minIndex;
        } else {
            break;
        }
    } while (1);
    return root;
}

int dijkstra(int grid[ROWS*EXTENDED_SIZE][COLS*EXTENDED_SIZE], int rows, int cols) {
    PriorityQueue pq;
    pq.elements = (Position*)malloc(sizeof(Position) * rows * cols);
    pq.size = 0;
    pq.capacity = rows * cols;
    pqPush(&pq, (Position){0, 0, 0});

    int dist[ROWS*EXTENDED_SIZE][COLS*EXTENDED_SIZE];
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            dist[i][j] = INT_MAX;
        }
    }
    dist[0][0] = 0;

    int dx[4] = {1, 0, -1, 0};
    int dy[4] = {0, 1, 0, -1};

    while (pq.size > 0) {
        Position curr = pqPop(&pq);
        if (curr.x == rows - 1 && curr.y == cols - 1) {
            free(pq.elements);
            return curr.risk;
        }
        for (int i = 0; i < 4; i++) {
            int nx = curr.x + dx[i];
            int ny = curr.y + dy[i];
            if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
                int nextRisk = curr.risk + grid[nx][ny];
                if (nextRisk < dist[nx][ny]) {
                    dist[nx][ny] = nextRisk;
                    pqPush(&pq, (Position){nx, ny, nextRisk});
                }
            }
        }
    }
    free(pq.elements);
    return -1;
}

void extendGrid(int initialGrid[ROWS][COLS], int extendedGrid[ROWS*EXTENDED_SIZE][COLS*EXTENDED_SIZE], int rows, int cols) {
    for (int i = 0; i < rows * EXTENDED_SIZE; i++) {
        for (int j = 0; j < cols * EXTENDED_SIZE; j++) {
            int newRisk = initialGrid[i % rows][j % cols] + i / rows + j / cols;
            while (newRisk > 9) newRisk -= 9;
            extendedGrid[i][j] = newRisk;
        }
    }
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    int initialGrid[ROWS][COLS];
    int extendedGrid[ROWS*EXTENDED_SIZE][COLS*EXTENDED_SIZE];
    char line[COLS + 2]; // +2 for newline and null terminator
    int rows = 0, cols = 0;

    while (fgets(line, sizeof(line), file)) {
        cols = 0;
        for (int i = 0; line[i] != '\n' && line[i] != '\0'; i++) {
            initialGrid[rows][cols++] = line[i] - '0';
        }
        rows++;
    }
    fclose(file);

    extendGrid(initialGrid, extendedGrid, rows, cols);

    printf("%d\n", dijkstra(extendedGrid, rows * EXTENDED_SIZE, cols * EXTENDED_SIZE));

    return 0;
}
