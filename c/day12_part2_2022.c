#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 1000

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point obj;
    int priority;
} Item;

typedef struct {
    Item* data;
    int size;
    int capacity;
} PriorityQueue;

Point neighbors4[] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

void initPriorityQueue(PriorityQueue* pq) {
    pq->data = (Item*)malloc(sizeof(Item) * MAX);
    pq->size = 0;
    pq->capacity = MAX;
}

void push(PriorityQueue* pq, Point obj, int priority) {
    if (pq->size == pq->capacity) {
        pq->capacity *= 2;
        pq->data = (Item*)realloc(pq->data, sizeof(Item) * pq->capacity);
    }
    pq->data[pq->size].obj = obj;
    pq->data[pq->size].priority = priority;
    int i = pq->size;
    while (i > 0) {
        int parent = (i - 1) / 2;
        if (pq->data[parent].priority <= pq->data[i].priority) break;
        Item temp = pq->data[parent];
        pq->data[parent] = pq->data[i];
        pq->data[i] = temp;
        i = parent;
    }
    pq->size++;
}

Item pop(PriorityQueue* pq) {
    Item min = pq->data[0];
    pq->data[0] = pq->data[--pq->size];
    int i = 0;
    while (1) {
        int left = 2 * i + 1;
        int right = 2 * i + 2;
        int minIndex = i;
        if (left < pq->size && pq->data[left].priority < pq->data[minIndex].priority) {
            minIndex = left;
        }
        if (right < pq->size && pq->data[right].priority < pq->data[minIndex].priority) {
            minIndex = right;
        }
        if (minIndex == i) break;
        Item temp = pq->data[minIndex];
        pq->data[minIndex] = pq->data[i];
        pq->data[i] = temp;
        i = minIndex;
    }
    return min;
}

int min(int a, int b) {
    return a < b ? a : b;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    char grid[MAX][MAX];
    int y = 0;
    Point start, end;
    Point as[MAX];
    int asSize = 0;
    while (fgets(grid[y], MAX, file)) {
        for (int x = 0; grid[y][x]; x++) {
            if (grid[y][x] == 'S') {
                start.x = x;
                start.y = y;
            } else if (grid[y][x] == 'E') {
                end.x = x;
                end.y = y;
            } else if (grid[y][x] == 'a') {
                as[asSize].x = x;
                as[asSize].y = y;
                asSize++;
            }
        }
        y++;
    }
    fclose(file);

    grid[start.y][start.x] = 'a';
    grid[end.y][end.x] = 'z';

    int dist[MAX][MAX];
    for (int i = 0; i < y; i++) {
        for (int j = 0; j < MAX; j++) {
            dist[i][j] = -1;
        }
    }
    dist[end.y][end.x] = 0;

    PriorityQueue pq;
    initPriorityQueue(&pq);
    push(&pq, end, 0);

    while (pq.size > 0) {
        Item item = pop(&pq);
        Point curr = item.obj;
        for (int i = 0; i < 4; i++) {
            Point next;
            next.x = curr.x + neighbors4[i].x;
            next.y = curr.y + neighbors4[i].y;
            if (next.x < 0 || next.x >= MAX || next.y < 0 || next.y >= y) {
                continue;
            }
            if (grid[next.y][next.x] == '\0') {
                continue;
            }
            if (grid[curr.y][curr.x] - grid[next.y][next.x] > 1) {
                continue;
            }
            int nextdist = dist[curr.y][curr.x] + 1;
            if (dist[next.y][next.x] == -1 || nextdist < dist[next.y][next.x]) {
                dist[next.y][next.x] = nextdist;
                push(&pq, next, nextdist);
            }
        }
    }

    int l = dist[start.y][start.x];
    for (int i = 0; i < asSize; i++) {
        if (dist[as[i].y][as[i].x] != -1) {
            l = min(l, dist[as[i].y][as[i].x]);
        }
    }
    printf("%d\n", l);

    return 0;
}