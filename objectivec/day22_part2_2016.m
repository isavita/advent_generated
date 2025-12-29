
#import <Foundation/Foundation.h>

typedef struct { int x, y; } P;
typedef struct { int u, a; } N;

static N g[100][100];
static int MX = 0, MY = 0;

int bfs(P goal, P src, P dst) {
    if (src.x == dst.x && src.y == dst.y) return 0;
    NSMutableArray *q = [NSMutableArray arrayWithObject:@[@(src.x), @(src.y), @(0)]];
    int d[100][100];
    for (int i = 0; i < 100; i++) for (int j = 0; j < 100; j++) d[i][j] = -1;
    d[src.x][src.y] = 0;
    int dx[] = {0, 0, 1, -1}, dy[] = {1, -1, 0, 0};
    while (q.count) {
        NSArray *c = q[0];
        [q removeObjectAtIndex:0];
        int cx = [c[0] intValue], cy = [c[1] intValue], cd = [c[2] intValue];
        if (cx == dst.x && cy == dst.y) return cd;
        for (int i = 0; i < 4; i++) {
            int nx = cx + dx[i], ny = cy + dy[i];
            if (nx < 0 || ny < 0 || nx > MX || ny > MY || g[nx][ny].u > 400 || (nx == goal.x && ny == goal.y)) continue;
            if (d[nx][ny] == -1) {
                d[nx][ny] = cd + 1;
                [q addObject:@[@(nx), @(ny), @(cd + 1)]];
            }
        }
    }
    return 1e8;
}

int main() {
    @autoreleasepool {
        NSString *s = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!s) return 0;
        P h = {0, 0};
        for (NSString *line in [s componentsSeparatedByString:@"\n"]) {
            int x, y, u, a, v;
            if (sscanf(line.UTF8String, "/dev/grid/node-x%d-y%d %dT %dT %dT", &x, &y, &v, &u, &a) == 5) {
                g[x][y] = (N){u, a};
                if (u == 0) h = (P){x, y};
                if (x > MX) MX = x;
                if (y > MY) MY = y;
            }
        }
        P d = {MX, 0};
        long long t = bfs(d, h, (P){MX - 1, 0}) + 1;
        h = (P){MX, 0};
        d = (P){MX - 1, 0};
        for (int i = 0; i < MX - 1; i++) {
            t += bfs(d, h, (P){d.x - 1, 0}) + 1;
            h = d;
            d = (P){d.x - 1, 0};
        }
        printf("%lld\n", t);
    }
    return 0;
}

