
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
    int X;
    int Y;
} Coord;

Coord add(Coord c1, Coord c2) {
    Coord res = {c1.X + c2.X, c1.Y + c2.Y};
    return res;
}

Coord multiplyByScalar(Coord c, int s) {
    Coord res = {c.X * s, c.Y * s};
    return res;
}

int absInt(int x) {
    return (x < 0) ? -x : x;
}

Coord directions[4] = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}}; // North, West, South, East

Coord parseDirection(char dir) {
    switch(dir) {
        case 'U': return directions[0];
        case 'L': return directions[1];
        case 'D': return directions[2];
        case 'R': return directions[3];
    }
    Coord invalid = {0, 0};
    return invalid; // Should never happen
}

int hexStringToInt(char *hexStr) {
    int num;
    sscanf(hexStr, "%x", &num);
    return num;
}

int shoelace(Coord *vertices, int n) {
    int area = 0;
    for (int i = 0; i < n; i++) {
        int next = (i + 1) % n;
        area += vertices[i].X * vertices[next].Y;
        area -= vertices[i].Y * vertices[next].X;
    }
    return absInt(area) / 2;
}

int perimeter(Coord *vertices, int n) {
    int perim = 0;
    for (int i = 0; i < n; i++) {
        int next = (i + 1) % n;
        perim += absInt(vertices[i].X - vertices[next].X) + absInt(vertices[i].Y - vertices[next].Y);
    }
    return perim;
}

int calculatePolygonArea(Coord *vertices, int n) {
    return shoelace(vertices, n) + perimeter(vertices, n) / 2 + 1;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Coord vertices[1000]; // Assuming the max number of vertices won't exceed 1000
    int vertexCount = 1;
    vertices[0].X = 0;
    vertices[0].Y = 0;

    char line[256];
    while (fgets(line, sizeof(line), file)) {
        char dirInput;
        char lengthStr[256];
        sscanf(line, "%c %s", &dirInput, lengthStr);
        int length = atoi(lengthStr);

        Coord dir = parseDirection(dirInput);
        vertices[vertexCount] = add(vertices[vertexCount - 1], multiplyByScalar(dir, length));
        vertexCount++;
    }

    fclose(file);

    printf("%d\n", calculatePolygonArea(vertices, vertexCount));

    return 0;
}
