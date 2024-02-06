
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    double x, y, z;
} Coord;

typedef struct {
    Coord pos;
    Coord vel;
} Point;

int parseInput(FILE* file, Point** points) {
    char* line = NULL;
    size_t len = 0;
    ssize_t read;
    int count = 0;

    while ((read = getline(&line, &len, file)) != -1) {
        count++;
    }

    fseek(file, 0, SEEK_SET);
    *points = (Point*)malloc(count * sizeof(Point));

    int i = 0;
    while ((read = getline(&line, &len, file)) != -1) {
        sscanf(line, "%lf, %lf, %lf @ %lf, %lf, %lf",
               &((*points)[i].pos.x), &((*points)[i].pos.y), &((*points)[i].pos.z),
               &((*points)[i].vel.x), &((*points)[i].vel.y), &((*points)[i].vel.z));
        i++;
    }
    free(line);
    return count;
}

int isIntersecting2D(Point p1, Point p2, Coord* coord, double* time1, double* time2) {
    double det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y;
    if (det == 0) {
        return 0;
    }

    *time1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det;
    *time2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det;

    coord->x = p1.pos.x + p1.vel.x * (*time1);
    coord->y = p1.pos.y + p1.vel.y * (*time1);
    coord->z = 0;

    return 1;
}

int solve(Point* points, int count, double min, double max) {
    int cnt = 0;
    for (int i = 0; i < count; i++) {
        for (int j = 0; j < i; j++) {
            Coord coord;
            double time1, time2;
            if (isIntersecting2D(points[i], points[j], &coord, &time1, &time2)) {
                int isInBound = min <= coord.x && coord.x <= max && min <= coord.y && coord.y <= max;
                if (isInBound && time1 >= 0 && time2 >= 0) {
                    cnt++;
                }
            }
        }
    }
    return cnt;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Unable to open file");
        return 1;
    }

    Point* points;
    int count = parseInput(file, &points);
    fclose(file);

    int result = solve(points, count, 200000000000000.0, 400000000000000.0);
    printf("%d\n", result);

    free(points);
    return 0;
}
