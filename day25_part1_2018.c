
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y, z, t;
} Point;

int abs(int x) {
    return x < 0 ? -x : x;
}

int manhattanDistance(Point a, Point b) {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t);
}

typedef struct {
    int *parent;
} UnionFind;

UnionFind* newUnionFind(int size) {
    UnionFind* uf = (UnionFind*)malloc(sizeof(UnionFind));
    uf->parent = (int*)malloc(size * sizeof(int));
    for (int i = 0; i < size; i++) {
        uf->parent[i] = i;
    }
    return uf;
}

int find(UnionFind* uf, int x) {
    if (uf->parent[x] != x) {
        uf->parent[x] = find(uf, uf->parent[x]);
    }
    return uf->parent[x];
}

void unionSets(UnionFind* uf, int x, int y) {
    int rootX = find(uf, x);
    int rootY = find(uf, y);
    if (rootX != rootY) {
        uf->parent[rootX] = rootY;
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Point *points = NULL;
    int numPoints = 0;
    char line[100];
    while (fgets(line, sizeof(line), file) != NULL) {
        int x, y, z, t;
        sscanf(line, "%d,%d,%d,%d", &x, &y, &z, &t);
        points = (Point*)realloc(points, (numPoints + 1) * sizeof(Point));
        points[numPoints].x = x;
        points[numPoints].y = y;
        points[numPoints].z = z;
        points[numPoints].t = t;
        numPoints++;
    }
    fclose(file);

    UnionFind *uf = newUnionFind(numPoints);
    for (int i = 0; i < numPoints; i++) {
        for (int j = 0; j < numPoints; j++) {
            if (manhattanDistance(points[i], points[j]) <= 3) {
                unionSets(uf, i, j);
            }
        }
    }

    int constellationCount = 0;
    for (int i = 0; i < numPoints; i++) {
        if (i == uf->parent[i]) {
            constellationCount++;
        }
    }
    printf("%d\n", constellationCount);

    free(uf->parent);
    free(uf);
    free(points);

    return 0;
}
