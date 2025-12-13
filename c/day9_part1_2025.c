
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 10000

typedef struct { int x, y; } Point;

static inline int absi(int x) { return x < 0 ? -x : x; }

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) { perror("input.txt"); return 1; }

    Point pts[MAX];
    int n = 0;
    while (n < MAX && fscanf(f, "%d,%d ", &pts[n].x, &pts[n].y) == 2) ++n;
    fclose(f);

    long long best = 0;
    for (int i = 0; i < n; ++i) {
        int x1 = pts[i].x, y1 = pts[i].y;
        for (int j = i; j < n; ++j) {
            long long dx = absi(x1 - pts[j].x) + 1LL;
            long long dy = absi(y1 - pts[j].y) + 1LL;
            long long area = dx * dy;
            if (area > best) best = area;
        }
    }
    printf("%lld\n", best);
    return 0;
}
