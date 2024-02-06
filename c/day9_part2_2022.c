
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x;
    int y;
} Point;

typedef enum {
    N, E, S, W
} Dir;

int abs(int x) {
    return x < 0 ? -x : x;
}

int sign(int n) {
    if (n == 0) return 0;
    return n > 0 ? 1 : -1;
}

Point addPoint(Point a, Point b) {
    Point result = {a.x + b.x, a.y + b.y};
    return result;
}

Point dirToPoint(Dir d) {
    switch (d) {
        case N: return (Point){0, 1};
        case E: return (Point){1, 0};
        case S: return (Point){0, -1};
        case W: return (Point){-1, 0};
        default: return (Point){0, 0}; // Should not happen
    }
}

Dir dirFromByte(char b) {
    switch (b) {
        case 'N': case 'U': case '^': return N;
        case 'E': case 'R': case '>': return E;
        case 'S': case 'D': case 'v': return S;
        case 'W': case 'L': case '<': return W;
        default: return N; // Default case, should not happen
    }
}

Point next(Point head, Point tail) {
    if (abs(head.x - tail.x) <= 1 && abs(head.y - tail.y) <= 1) {
        return tail;
    }
    return addPoint(tail, (Point){sign(head.x - tail.x), sign(head.y - tail.y)});
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    const int ropelen = 10;
    Point rope[ropelen];
    memset(rope, 0, sizeof(rope)); // Initialize all points to {0, 0}

    char line[100];
    int visitedCount = 0;
    Point visited[10000]; // Assuming a maximum of 10000 unique points for simplicity

    while (fgets(line, sizeof(line), file) != NULL) {
        char b;
        int n;
        sscanf(line, "%c %d", &b, &n);
        Dir d = dirFromByte(b);

        for (int i = 0; i < n; i++) {
            rope[0] = addPoint(rope[0], dirToPoint(d));
            for (int j = 1; j < ropelen; j++) {
                rope[j] = next(rope[j-1], rope[j]);
            }

            // Check if point is unique (simple linear search)
            int unique = 1;
            for (int k = 0; k < visitedCount; k++) {
                if (visited[k].x == rope[ropelen-1].x && visited[k].y == rope[ropelen-1].y) {
                    unique = 0;
                    break;
                }
            }
            if (unique) {
                visited[visitedCount++] = rope[ropelen-1];
            }
        }
    }
    fclose(file);

    printf("%d\n", visitedCount);
    return 0;
}
