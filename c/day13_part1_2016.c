
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int x, y;
} Point;

int isWall(int favoriteNumber, int x, int y) {
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

int bfs(Point start, Point target, int favoriteNumber) {
    int steps = 0;
    Point queue[1000];
    int front = 0, rear = 0;
    Point visited[1000];

    queue[rear++] = start;

    while (front < rear) {
        int size = rear - front;
        for (int i = 0; i < size; i++) {
            Point point = queue[front++];
            if (point.x == target.x && point.y == target.y) {
                return steps;
            }

            Point deltas[] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
            for (int j = 0; j < 4; j++) {
                Point delta = deltas[j];
                Point next = {point.x + delta.x, point.y + delta.y};
                if (next.x >= 0 && next.y >= 0 && !isWall(favoriteNumber, next.x, next.y)) {
                    int found = 0;
                    for (int k = 0; k < rear; k++) {
                        if (visited[k].x == next.x && visited[k].y == next.y) {
                            found = 1;
                            break;
                        }
                    }
                    if (!found) {
                        visited[rear] = next;
                        queue[rear++] = next;
                    }
                }
            }
        }
        steps++;
    }

    return -1;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char num[10];
    fgets(num, 10, file);
    int favoriteNumber = atoi(num);

    Point start = {1, 1};
    Point target = {31, 39};

    int steps = bfs(start, target, favoriteNumber);
    printf("%d\n", steps);

    fclose(file);
    return 0;
}
