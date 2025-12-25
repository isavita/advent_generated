
#import <Foundation/Foundation.h>

typedef struct { int r, c; } Pos;

static inline int idx(int r, int c, int w) { return r * w + c; }

int *bfs(Pos start, int h, int w, const bool *walls) {
    int *dist = calloc(h * w, sizeof(int));
    for (int i = 0; i < h * w; ++i) dist[i] = -1;
    Pos *q = malloc(h * w * sizeof(Pos));
    int head = 0, tail = 0;
    dist[idx(start.r, start.c, w)] = 0;
    q[tail++] = start;
    int dr[4] = {1,-1,0,0}, dc[4] = {0,0,1,-1};
    while (head < tail) {
        Pos cur = q[head++];
        int curd = dist[idx(cur.r, cur.c, w)];
        for (int i = 0; i < 4; ++i) {
            int nr = cur.r + dr[i], nc = cur.c + dc[i];
            if (nr < 0 || nr >= h || nc < 0 || nc >= w) continue;
            if (walls[idx(nr,nc,w)]) continue;
            int ni = idx(nr,nc,w);
            if (dist[ni] == -1) {
                dist[ni] = curd + 1;
                q[tail++] = (Pos){nr,nc};
            }
        }
    }
    free(q);
    return dist;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSString *> *grid = [NSMutableArray array];
        for (NSString *ln in lines) if (ln.length) [grid addObject:ln];
        int h = (int)grid.count;
        if (h == 0) { printf("0\n"); return 0; }
        int w = (int)[grid[0] length];
        if (w == 0) { printf("0\n"); return 0; }

        bool *walls = calloc(h * w, sizeof(bool));
        Pos S = {0,0}, E = {0,0};
        Pos *track = malloc(h * w * sizeof(Pos));
        int trackCount = 0;
        for (int r = 0; r < h; ++r) {
            const char *row = [grid[r] UTF8String];
            for (int c = 0; c < w; ++c) {
                char ch = row[c];
                if (ch == 'S') S = (Pos){r,c};
                else if (ch == 'E') E = (Pos){r,c};
                if (ch == '#') walls[idx(r,c,w)] = true;
                else track[trackCount++] = (Pos){r,c};
            }
        }

        int *distS = bfs(S, h, w, walls);
        int *distE = bfs(E, h, w, walls);
        int normal = distS[idx(E.r,E.c,w)];
        if (normal == -1) { printf("0\n"); return 0; }

        int dr[4] = {1,-1,0,0}, dc[4] = {0,0,1,-1};
        int cheats = 0;
        for (int ti = 0; ti < trackCount; ++ti) {
            Pos p = track[ti];
            int sd = distS[idx(p.r,p.c,w)];
            if (sd == -1) continue;
            for (int i = 0; i < 4; ++i) {
                int m1r = p.r + dr[i], m1c = p.c + dc[i];
                if (m1r < 0 || m1r >= h || m1c < 0 || m1c >= w) continue;
                for (int j = 0; j < 4; ++j) {
                    int m2r = m1r + dr[j], m2c = m1c + dc[j];
                    if (m2r < 0 || m2r >= h || m2c < 0 || m2c >= w) continue;
                    if (walls[idx(m2r,m2c,w)]) continue;
                    int ed = distE[idx(m2r,m2c,w)];
                    if (ed == -1) continue;
                    int newCost = sd + 2 + ed;
                    if (normal - newCost >= 100) ++cheats;
                }
            }
        }
        printf("%d\n", cheats);
        free(walls); free(track); free(distS); free(distE);
    }
    return 0;
}
