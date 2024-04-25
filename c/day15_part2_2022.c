#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y, dist;
} sensor;

int abs(int n) {
    return n < 0 ? -n : n;
}

int manhattan(int x1, int y1, int x2, int y2) {
    return abs(x1 - x2) + abs(y1 - y2);
}

void distress(sensor* sensors, int n, int maxcoord) {
    for (int x = 0; x <= maxcoord; x++) {
        int y = 0;
        while (y <= maxcoord) {
            int detected = 0;
            for (int i = 0; i < n; i++) {
                if (manhattan(sensors[i].x, sensors[i].y, x, y) <= sensors[i].dist) {
                    detected = 1;
                    int dist = sensors[i].dist - abs(sensors[i].x - x);
                    y = sensors[i].y + dist + 1;
                    break;
                }
            }
            if (!detected) {
                printf("%lld\n", (long long)x * 4000000 + y);
                return;
            }
        }
    }
    printf("-1\n");
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    int n;
    fscanf(file, "%d", &n);

    sensor* sensors = (sensor*)malloc(n * sizeof(sensor));

    for (int i = 0; i < n; i++) {
        int x, y, bx, by;
        fscanf(file, "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d\n", &x, &y, &bx, &by);
        sensors[i].x = x;
        sensors[i].y = y;
        sensors[i].dist = manhattan(x, y, bx, by);
    }

    fclose(file);

    distress(sensors, n, 4000000);

    free(sensors);

    return 0;
}