
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int X, Y;
} Point;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char line[100];
    Point points[1000];
    int pointCount = 0;
    int folds[10][2]; // Assuming a maximum of 10 folds for simplicity
    int foldCount = 0;
    int readingPoints = 1;

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n') {
            readingPoints = 0;
            continue;
        }
        if (readingPoints) {
            int x, y;
            sscanf(line, "%d,%d", &x, &y);
            points[pointCount++] = (Point){x, y};
        } else {
            char axis;
            int value;
            sscanf(line, "fold along %c=%d", &axis, &value);
            folds[foldCount][0] = axis == 'x' ? 0 : 1; // 0 for x, 1 for y
            folds[foldCount++][1] = value;
        }
    }

    fclose(file);

    // Process the first fold instruction
    int axis = folds[0][0];
    int value = folds[0][1];

    for (int i = 0; i < pointCount; i++) {
        if (axis == 0 && points[i].X > value) { // x-axis
            points[i].X = 2 * value - points[i].X;
        } else if (axis == 1 && points[i].Y > value) { // y-axis
            points[i].Y = 2 * value - points[i].Y;
        }
    }

    // Count unique points
    int uniqueCount = 0;
    for (int i = 0; i < pointCount; i++) {
        int unique = 1;
        for (int j = 0; j < i; j++) {
            if (points[i].X == points[j].X && points[i].Y == points[j].Y) {
                unique = 0;
                break;
            }
        }
        uniqueCount += unique;
    }

    printf("%d\n", uniqueCount);

    return 0;
}
