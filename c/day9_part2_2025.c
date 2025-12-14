
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct { int x, y; } Point;

static int cmp_int(const void *a, const void *b) {
    return *(int *)a - *(int *)b;
}
static int abs_i(int a) { return a < 0 ? -a : a; }

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;

    Point *pts = NULL;
    size_t pts_cap = 0, pts_sz = 0;
    int *xs = NULL, *ys = NULL;
    size_t xs_cap = 0, ys_cap = 0, xs_sz = 0, ys_sz = 0;
    char line[64];
    while (fgets(line, sizeof line, f)) {
        int x, y;
        if (sscanf(line, " %d , %d ", &x, &y) != 2) continue;
        if (pts_sz == pts_cap) {
            pts_cap = pts_cap ? pts_cap * 2 : 64;
            pts = realloc(pts, pts_cap * sizeof *pts);
        }
        pts[pts_sz++] = (Point){x, y};
        if (xs_sz == xs_cap) {
            xs_cap = xs_cap ? xs_cap * 2 : 64;
            xs = realloc(xs, xs_cap * sizeof *xs);
        }
        xs[xs_sz++] = x;
        if (ys_sz == ys_cap) {
            ys_cap = ys_cap ? ys_cap * 2 : 64;
            ys = realloc(ys, ys_cap * sizeof *ys);
        }
        ys[ys_sz++] = y;
    }
    fclose(f);
    if (pts_sz == 0) {
        printf("Largest valid area: 0\n");
        return 0;
    }

    qsort(xs, xs_sz, sizeof *xs, cmp_int);
    qsort(ys, ys_sz, sizeof *ys, cmp_int);
    size_t ux_sz = 0, uy_sz = 0;
    for (size_t i = 0; i < xs_sz; ++i)
        if (i == 0 || xs[i] != xs[i - 1]) xs[ux_sz++] = xs[i];
    for (size_t i = 0; i < ys_sz; ++i)
        if (i == 0 || ys[i] != ys[i - 1]) ys[uy_sz++] = ys[i];

    int *xidx = malloc((int)(xs[ux_sz - 1] + 1) * sizeof *xidx);
    int *yidx = malloc((int)(ys[uy_sz - 1] + 1) * sizeof *yidx);
    for (size_t i = 0; i < ux_sz; ++i) xidx[xs[i]] = (int)i;
    for (size_t i = 0; i < uy_sz; ++i) yidx[ys[i]] = (int)i;

    int W = (int)(2 * ux_sz + 1);
    int H = (int)(2 * uy_sz + 1);
    int64_t *colW = calloc(W, sizeof *colW);
    int64_t *rowH = calloc(H, sizeof *rowH);
    colW[0] = 1;
    for (size_t i = 0; i < ux_sz; ++i) {
        colW[2 * i + 1] = 1;
        if (i + 1 < ux_sz) {
            int64_t gap = xs[i + 1] - xs[i] - 1;
            if (gap < 0) gap = 0;
            colW[2 * i + 2] = gap;
        } else colW[2 * i + 2] = 1;
    }
    rowH[0] = 1;
    for (size_t i = 0; i < uy_sz; ++i) {
        rowH[2 * i + 1] = 1;
        if (i + 1 < uy_sz) {
            int64_t gap = ys[i + 1] - ys[i] - 1;
            if (gap < 0) gap = 0;
            rowH[2 * i + 2] = gap;
        } else rowH[2 * i + 2] = 1;
    }

    int8_t **grid = malloc(H * sizeof *grid);
    for (int y = 0; y < H; ++y) {
        grid[y] = calloc(W, sizeof *grid[y]);
    }

    #define TOGRID(px,py) (2 * xidx[(px)] + 1), (2 * yidx[(py)] + 1)

    for (size_t i = 0; i < pts_sz; ++i) {
        Point a = pts[i];
        Point b = pts[(i + 1) % pts_sz];
        int gx1, gy1, gx2, gy2;
        gx1 = 2 * xidx[a.x] + 1; gy1 = 2 * yidx[a.y] + 1;
        gx2 = 2 * xidx[b.x] + 1; gy2 = 2 * yidx[b.y] + 1;
        if (gx1 == gx2) {
            int y0 = gy1 < gy2 ? gy1 : gy2;
            int y1 = gy1 > gy2 ? gy1 : gy2;
            for (int y = y0; y <= y1; ++y) if (rowH[y] > 0) grid[y][gx1] = 1;
        } else {
            int x0 = gx1 < gx2 ? gx1 : gx2;
            int x1 = gx1 > gx2 ? gx1 : gx2;
            for (int x = x0; x <= x1; ++x) if (colW[x] > 0) grid[gy1][x] = 1;
        }
    }

    Point *queue = malloc(W * H * sizeof *queue);
    size_t qh = 0, qt = 0;
    queue[qt++] = (Point){0,0};
    grid[0][0] = 2;
    const int dirs[4][2] = {{0,1},{0,-1},{1,0},{-1,0}};
    while (qh < qt) {
        Point cur = queue[qh++];
        for (int d = 0; d < 4; ++d) {
            int nx = cur.x + dirs[d][0];
            int ny = cur.y + dirs[d][1];
            if (nx >= 0 && nx < W && ny >= 0 && ny < H && grid[ny][nx] == 0) {
                grid[ny][nx] = 2;
                queue[qt++] = (Point){nx, ny};
            }
        }
    }
    free(queue);

    int64_t **P = malloc(H * sizeof *P);
    for (int y = 0; y < H; ++y) {
        P[y] = calloc(W, sizeof *P[y]);
        for (int x = 0; x < W; ++x) {
            int64_t val = (grid[y][x] != 2) ? colW[x] * rowH[y] : 0;
            int64_t left = x ? P[y][x-1] : 0;
            int64_t up = y ? P[y-1][x] : 0;
            int64_t diag = (x && y) ? P[y-1][x-1] : 0;
            P[y][x] = val + left + up - diag;
        }
    }

    int64_t maxArea = 0;
    for (size_t i = 0; i < pts_sz; ++i) {
        for (size_t j = i; j < pts_sz; ++j) {
            Point a = pts[i], b = pts[j];
            int64_t w = (int64_t)abs_i(a.x - b.x) + 1;
            int64_t h = (int64_t)abs_i(a.y - b.y) + 1;
            int64_t area = w * h;
            if (area <= maxArea) continue;
            int gx1 = 2 * xidx[a.x] + 1, gy1 = 2 * yidx[a.y] + 1;
            int gx2 = 2 * xidx[b.x] + 1, gy2 = 2 * yidx[b.y] + 1;
            if (gx1 > gx2) { int t = gx1; gx1 = gx2; gx2 = t; }
            if (gy1 > gy2) { int t = gy1; gy1 = gy2; gy2 = t; }
            int64_t total = P[gy2][gx2];
            int64_t left = gx1 ? P[gy2][gx1-1] : 0;
            int64_t up = gy1 ? P[gy1-1][gx2] : 0;
            int64_t diag = (gx1 && gy1) ? P[gy1-1][gx1-1] : 0;
            int64_t valid = total - left - up + diag;
            if (valid == area) maxArea = area;
        }
    }

    printf("Largest valid area: %lld\n", (long long)maxArea);

    for (int y = 0; y < H; ++y) free(grid[y]), free(P[y]);
    free(grid); free(P); free(colW); free(rowH);
    free(pts); free(xs); free(ys); free(xidx); free(yidx);
    return 0;
}
