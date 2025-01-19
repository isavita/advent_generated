
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_GRID_SIZE 200
#define MAX_NODES MAX_GRID_SIZE * MAX_GRID_SIZE

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point p;
    int priority;
} Item;

typedef struct {
    Item* data;
    int size;
    int capacity;
} PriorityQueue;

int grid[MAX_GRID_SIZE][MAX_GRID_SIZE];
int dist[MAX_GRID_SIZE][MAX_GRID_SIZE];
int rows, cols;

PriorityQueue* createPriorityQueue(int capacity) {
    PriorityQueue* pq = (PriorityQueue*)malloc(sizeof(PriorityQueue));
    pq->data = (Item*)malloc(sizeof(Item) * capacity);
    pq->size = 0;
    pq->capacity = capacity;
    return pq;
}

void freePriorityQueue(PriorityQueue* pq) {
    free(pq->data);
    free(pq);
}


void swap(Item* a, Item* b) {
    Item temp = *a;
    *a = *b;
    *b = temp;
}

void heapifyDown(PriorityQueue* pq, int i) {
    int largest = i;
    int left = 2 * i + 1;
    int right = 2 * i + 2;

    if (left < pq->size && pq->data[left].priority > pq->data[largest].priority) {
        largest = left;
    }

    if (right < pq->size && pq->data[right].priority > pq->data[largest].priority) {
        largest = right;
    }

    if (largest != i) {
        swap(&pq->data[i], &pq->data[largest]);
        heapifyDown(pq, largest);
    }
}

void heapifyUp(PriorityQueue* pq, int i) {
    while(i > 0 && pq->data[(i - 1) / 2].priority < pq->data[i].priority) {
      swap(&pq->data[i], &pq->data[(i - 1) / 2]);
      i = (i - 1) / 2;
    }
}

void push(PriorityQueue* pq, Point p, int priority) {
    if (pq->size == pq->capacity) {
        pq->capacity *= 2;
        pq->data = (Item*)realloc(pq->data, sizeof(Item) * pq->capacity);
    }
    pq->data[pq->size].p = p;
    pq->data[pq->size].priority = priority;
    heapifyUp(pq, pq->size);
    pq->size++;
}

Item pop(PriorityQueue* pq) {
    Item item = pq->data[0];
    pq->data[0] = pq->data[pq->size - 1];
    pq->size--;
    heapifyDown(pq, 0);
    return item;
}


int isValid(int x, int y) {
    return x >= 0 && x < cols && y >= 0 && y < rows;
}

int dx[] = {0, 0, 1, -1};
int dy[] = {1, -1, 0, 0};

int dijkstra(Point end, int start_x, int start_y) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
           dist[i][j] = INT_MAX;
        }
    }
    PriorityQueue* pq = createPriorityQueue(MAX_NODES);
    dist[end.y][end.x] = 0;
    push(pq, end, 0);

    while (pq->size > 0) {
        Item current_item = pop(pq);
        Point curr = current_item.p;
        int curr_dist = current_item.priority;

        if (curr_dist > dist[curr.y][curr.x]) {
          continue;
        }

        for (int i = 0; i < 4; i++) {
            int next_x = curr.x + dx[i];
            int next_y = curr.y + dy[i];

            if (isValid(next_x, next_y) &&
               grid[curr.y][curr.x] - grid[next_y][next_x] <= 1)
            {
              int next_dist = curr_dist + 1;
               if (next_dist < dist[next_y][next_x]) {
                  dist[next_y][next_x] = next_dist;
                  push(pq, (Point){next_x, next_y}, next_dist);
              }
            }
        }
    }
    freePriorityQueue(pq);
    return dist[start_y][start_x];
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_GRID_SIZE];
    rows = 0;
    Point start, end;
    int start_x, start_y;
    while (fgets(line, sizeof(line), file)) {
        cols = strlen(line) - 1;
        for (int x = 0; x < cols; x++) {
            grid[rows][x] = line[x];
            if (line[x] == 'S') {
                start_x = x;
                start_y = rows;
            } else if (line[x] == 'E') {
                end.x = x;
                end.y = rows;
            }
        }
         rows++;
    }
    fclose(file);
    grid[start_y][start_x] = 'a';
    grid[end.y][end.x] = 'z';


    int shortest_path = dijkstra(end,start_x, start_y);

    printf("%d\n", shortest_path);
    return 0;
}
