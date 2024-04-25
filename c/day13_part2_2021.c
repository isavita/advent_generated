#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_POINTS 1000
#define MAX_FOLDS 100
#define MAX_GRID 1000

typedef struct {
    int x, y;
} Point;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    Point points[MAX_POINTS];
    int pointCount = 0;
    Point folds[MAX_FOLDS];
    int foldCount = 0;
    int readingPoints = 1;

    char line[100];
    while (fgets(line, sizeof(line), file)) {
        if (strcmp(line, "\n") == 0) {
            readingPoints = 0;
            continue;
        }
        if (readingPoints) {
            int x, y;
            sscanf(line, "%d,%d", &x, &y);
            points[pointCount].x = x;
            points[pointCount].y = y;
            pointCount++;
        } else {
            char axis;
            int val;
            sscanf(line, "fold along %c=%d", &axis, &val);
            if (axis == 'x') {
                folds[foldCount].x = val;
                folds[foldCount].y = 0;
            } else {
                folds[foldCount].x = 0;
                folds[foldCount].y = val;
            }
            foldCount++;
        }
    }
    fclose(file);

    for (int i = 0; i < foldCount; i++) {
        Point newPoints[MAX_POINTS];
        int newPointCount = 0;
        for (int j = 0; j < pointCount; j++) {
            Point newPoint = points[j];
            if (folds[i].x != 0 && points[j].x > folds[i].x) {
                newPoint.x = folds[i].x - (points[j].x - folds[i].x);
            } else if (folds[i].y != 0 && points[j].y > folds[i].y) {
                newPoint.y = folds[i].y - (points[j].y - folds[i].y);
            }
            int exists = 0;
            for (int k = 0; k < newPointCount; k++) {
                if (newPoints[k].x == newPoint.x && newPoints[k].y == newPoint.y) {
                    exists = 1;
                    break;
                }
            }
            if (!exists) {
                newPoints[newPointCount] = newPoint;
                newPointCount++;
            }
        }
        pointCount = newPointCount;
        for (int j = 0; j < pointCount; j++) {
            points[j] = newPoints[j];
        }
        if (i == 0) {
            printf("Number of dots visible after first fold: %d\n", pointCount);
        }
    }

    int maxX = 0, maxY = 0;
    for (int i = 0; i < pointCount; i++) {
        if (points[i].x > maxX) {
            maxX = points[i].x;
        }
        if (points[i].y > maxY) {
            maxY = points[i].y;
        }
    }

    char grid[MAX_GRID][MAX_GRID];
    for (int i = 0; i <= maxY; i++) {
        for (int j = 0; j <= maxX; j++) {
            grid[i][j] = ' ';
        }
    }

    for (int i = 0; i < pointCount; i++) {
        grid[points[i].y][points[i].x] = '#';
    }

    for (int i = 0; i <= maxY; i++) {
        for (int j = 0; j <= maxX; j++) {
            printf("%c", grid[i][j]);
        }
        printf("\n");
    }

    return 0;
}