
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

typedef struct {
    int x;
    int y;
} Point;

typedef struct {
    Point pos;
    Point beacon;
    int dist;
} Sensor;

int abs(int n) {
    return n < 0 ? -n : n;
}

int manhattan(Point p, Point q) {
    return abs(p.x - q.x) + abs(p.y - q.y);
}

int impossible(Sensor *sensors, int sensorCount, int y) {
    int *pts = NULL;
    int ptsCount = 0;
    int max_x = INT_MIN;
    int min_x = INT_MAX;

    for (int i = 0; i < sensorCount; i++) {
        int dist = sensors[i].dist - abs(sensors[i].pos.y - y);
        if(dist < 0) continue;
        int startX = sensors[i].pos.x - dist;
        int endX = sensors[i].pos.x + dist;
        if(startX < min_x) min_x = startX;
        if(endX > max_x) max_x = endX;
    }


    if (min_x == INT_MAX || max_x == INT_MIN) return 0;

    int range = max_x - min_x + 1;
    char *covered = calloc(range, sizeof(char));

   for (int i = 0; i < sensorCount; i++) {
        int dist = sensors[i].dist - abs(sensors[i].pos.y - y);
       if(dist < 0) continue;
        int startX = sensors[i].pos.x - dist;
        int endX = sensors[i].pos.x + dist;

        for(int x = startX; x <= endX; x++){
           covered[x - min_x] = 1;
        }
    }


    for (int i = 0; i < sensorCount; i++) {
        if (sensors[i].beacon.y == y) {
            if (sensors[i].beacon.x >= min_x && sensors[i].beacon.x <= max_x)
               covered[sensors[i].beacon.x - min_x] = 0;
        }
    }

    for(int i=0; i < range; i++){
        if(covered[i]) ptsCount++;
    }


    free(covered);

    return ptsCount;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    Sensor sensors[1000];
    int sensorCount = 0;
    char line[256];

    while (fgets(line, sizeof(line), fp) != NULL) {
        sscanf(line, "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
               &sensors[sensorCount].pos.x, &sensors[sensorCount].pos.y,
               &sensors[sensorCount].beacon.x, &sensors[sensorCount].beacon.y);
        sensors[sensorCount].dist = manhattan(sensors[sensorCount].pos, sensors[sensorCount].beacon);
        sensorCount++;
    }

    fclose(fp);
    printf("%d\n", impossible(sensors, sensorCount, 2000000));
    return 0;
}
