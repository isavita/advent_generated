
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y, z;
} Point;

typedef struct {
    int u, v;
    long long d;
} Edge;

/* ---------- Union‑Find ---------- */
static int *parent;
static int *sz;

static int find_set(int x) {
    while (parent[x] != x) {
        parent[x] = parent[parent[x]];
        x = parent[x];
    }
    return x;
}

static void union_set(int a, int b) {
    int ra = find_set(a);
    int rb = find_set(b);
    if (ra == rb) return;
    if (sz[ra] < sz[rb]) {
        int tmp = ra; ra = rb; rb = tmp;
    }
    parent[rb] = ra;
    sz[ra] += sz[rb];
}

/* ---------- Edge comparison ---------- */
static int cmp_edge(const void *p, const void *q) {
    const Edge *a = p, *b = q;
    return (a->d < b->d) ? -1 : (a->d > b->d);
}

/* ---------- Main ---------- */
int main(void) {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) return 1;

    /* read points */
    Point *pts = NULL;
    size_t pts_cap = 0, n = 0;
    char line[256];
    while (fgets(line, sizeof line, fp)) {
        int x, y, z;
        if (sscanf(line, " %d , %d , %d", &x, &y, &z) != 3) continue;
        if (n == pts_cap) {
            pts_cap = pts_cap ? pts_cap * 2 : 128;
            pts = realloc(pts, pts_cap * sizeof *pts);
        }
        pts[n++] = (Point){x, y, z};
    }
    fclose(fp);
    if (n < 2) {
        printf("Not enough points to form circuits.\n");
        free(pts);
        return 0;
    }

    /* generate edges */
    size_t e_cnt = n * (n - 1) / 2;
    Edge *edges = malloc(e_cnt * sizeof *edges);
    size_t idx = 0;
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = i + 1; j < n; ++j) {
            long long dx = (long long)pts[i].x - pts[j].x;
            long long dy = (long long)pts[i].y - pts[j].y;
            long long dz = (long long)pts[i].z - pts[j].z;
            edges[idx++] = (Edge){(int)i, (int)j, dx * dx + dy * dy + dz * dz};
        }
    }

    /* sort edges by distance */
    qsort(edges, e_cnt, sizeof *edges, cmp_edge);

    /* init union‑find */
    parent = malloc(n * sizeof *parent);
    sz = malloc(n * sizeof *sz);
    for (size_t i = 0; i < n; ++i) {
        parent[i] = (int)i;
        sz[i] = 1;
    }

    /* connect the 1000 shortest edges */
    size_t limit = e_cnt < 1000 ? e_cnt : 1000;
    for (size_t i = 0; i < limit; ++i)
        union_set(edges[i].u, edges[i].v);

    /* find three largest component sizes */
    int top[3] = {0, 0, 0};
    for (size_t i = 0; i < n; ++i) {
        if (parent[i] == (int)i) {
            int s = sz[i];
            if (s > top[0]) {
                top[2] = top[1];
                top[1] = top[0];
                top[0] = s;
            } else if (s > top[1]) {
                top[2] = top[1];
                top[1] = s;
            } else if (s > top[2]) {
                top[2] = s;
            }
        }
    }

    unsigned long long result = 1;
    for (int i = 0; i < 3 && top[i] > 0; ++i)
        result *= (unsigned long long)top[i];

    printf("Product of three largest circuit sizes: %llu\n", result);

    free(pts);
    free(edges);
    free(parent);
    free(sz);
    return 0;
}
