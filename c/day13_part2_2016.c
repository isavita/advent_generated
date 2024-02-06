
#include <stdio.h>
#include <stdbool.h>

#define favoriteNumber 1362

typedef struct {
    int x, y;
} Point;

bool isWall(int x, int y) {
    int num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber;
    int bits = 0;
    while (num > 0) {
        if (num%2 == 1) {
            bits++;
        }
        num /= 2;
    }
    return bits%2 != 0;
}

int bfsMaxSteps(Point start, int maxSteps) {
    int steps = 0;
    Point queue[10000];
    bool visited[100][100] = {false};

    int front = 0, rear = 0;
    queue[rear++] = start;
    visited[start.x][start.y] = true;

    while (front < rear && steps < maxSteps) {
        int size = rear - front;
        for (int i = 0; i < size; i++) {
            Point point = queue[front++];

            Point deltas[] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
            for (int j = 0; j < 4; j++) {
                Point delta = deltas[j];
                Point next = {point.x + delta.x, point.y + delta.y};
                if (next.x >= 0 && next.y >= 0 && !isWall(next.x, next.y) && !visited[next.x][next.y]) {
                    visited[next.x][next.y] = true;
                    queue[rear++] = next;
                }
            }
        }
        steps++;
    }

    int count = 0;
    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            if (visited[i][j]) {
                count++;
            }
        }
    }

    return count;
}

int main() {
    Point start = {1, 1};
    int reachableLocations = bfsMaxSteps(start, 50);
    printf("%d\n", reachableLocations);

    return 0;
}
