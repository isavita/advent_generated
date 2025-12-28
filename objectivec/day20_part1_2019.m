
#import <Foundation/Foundation.h>

static NSArray<NSArray<NSString *>*> *readMaze(NSString *filename) {
    NSError *err;
    NSString *txt = [NSString stringWithContentsOfFile:filename
                                              encoding:NSUTF8StringEncoding
                                                 error:&err];
    if (!txt) return nil;
    NSMutableArray *m = [NSMutableArray array];
    for (NSString *line in [txt componentsSeparatedByString:@"\n"]) {
        if (line.length == 0) continue;
        NSMutableArray *row = [NSMutableArray array];
        for (int i = 0; i < line.length; i++)
            [row addObject:[line substringWithRange:NSMakeRange(i, 1)]];
        [m addObject:row];
    }
    return m;
}

static BOOL isUpper(NSString *c) {
    if (!c) return NO;
    unichar ch = [c characterAtIndex:0];
    return ch >= 'A' && ch <= 'Z';
}

static void findPortals(NSArray<NSArray<NSString *>*> *maze,
                       NSMutableDictionary<NSString *, NSMutableArray<NSArray<NSNumber *>*>*> **portals,
                       NSMutableDictionary<NSString *, NSString *> **portalPos) {
    NSUInteger h = maze.count, w = maze[0].count;
    *portals = [NSMutableDictionary dictionary];
    *portalPos = [NSMutableDictionary dictionary];
    for (NSUInteger y = 0; y < h; y++) {
        for (NSUInteger x = 0; x < w; x++) {
            NSString *c = maze[y][x];
            if (!isUpper(c)) continue;
            // right
            if (x + 1 < w && isUpper(maze[y][x+1])) {
                NSString *name = [NSString stringWithFormat:@"%@%@", c, maze[y][x+1]];
                long px = -1, py = -1;
                if (x + 2 < w && [maze[y][x+2] isEqualToString:@"."]) { px = x + 2; py = y; }
                else if (x > 0 && [maze[y][x-1] isEqualToString:@"."]) { px = x - 1; py = y; }
                if (px >= 0) {
                    if (!(*portals)[name]) (*portals)[name] = [NSMutableArray array];
                    [(*portals)[name] addObject:@[@(px), @(py)]];
                    (*portalPos)[[NSString stringWithFormat:@"%.0ld,%.0ld", px, py]] = name;
                }
            }
            // down
            if (y + 1 < h && isUpper(maze[y+1][x])) {
                NSString *name = [NSString stringWithFormat:@"%@%@", c, maze[y+1][x]];
                long px = -1, py = -1;
                if (y + 2 < h && [maze[y+2][x] isEqualToString:@"."]) { px = x; py = y + 2; }
                else if (y > 0 && [maze[y-1][x] isEqualToString:@"."]) { px = x; py = y - 1; }
                if (px >= 0) {
                    if (!(*portals)[name]) (*portals)[name] = [NSMutableArray array];
                    [(*portals)[name] addObject:@[@(px), @(py)]];
                    (*portalPos)[[NSString stringWithFormat:@"%.0ld,%.0ld", px, py]] = name;
                }
            }
        }
    }
}

static long bfs(NSArray<NSArray<NSString *>*> *maze,
                NSMutableDictionary<NSString *, NSMutableArray<NSArray<NSNumber *>*>*> *portals,
                NSMutableDictionary<NSString *, NSString *> *portalPos,
                NSArray<NSNumber *> *start,
                NSArray<NSNumber *> *end) {
    NSMutableArray *q = [NSMutableArray array];
    NSMutableSet *vis = [NSMutableSet set];
    [q addObject:@[start[0], start[1], @0]];
    [vis addObject:[NSString stringWithFormat:@"%@,%@", start[0], start[1]]];
    long w = maze[0].count, h = maze.count;
    long ex = [end[0] longValue], ey = [end[1] longValue];
    while (q.count) {
        NSArray *cur = q[0]; [q removeObjectAtIndex:0];
        long x = [cur[0] longValue], y = [cur[1] longValue], steps = [cur[2] longValue];
        long dxs[] = {-1,1,0,0}, dys[] = {0,0,-1,1};
        for (int d = 0; d < 4; d++) {
            long nx = x + dxs[d], ny = y + dys[d];
            if (nx < 0 || nx >= w || ny < 0 || ny >= h) continue;
            if (![maze[ny][nx] isEqualToString:@"."]) continue;
            if (nx == ex && ny == ey) return steps + 1;
            NSString *k = [NSString stringWithFormat:@"%.0ld,%.0ld", nx, ny];
            if ([vis containsObject:k]) continue;
            [vis addObject:k];
            [q addObject:@[@(nx), @(ny), @(steps + 1)]];
        }
        NSString *curK = [NSString stringWithFormat:@"%.0ld,%.0ld", x, y];
        NSString *name = portalPos[curK];
        if (name) {
            for (NSArray *dest in portals[name]) {
                long px = [dest[0] longValue], py = [dest[1] longValue];
                if (px == x && py == y) continue;
                NSString *k2 = [NSString stringWithFormat:@"%.0ld,%.0ld", px, py];
                if ([vis containsObject:k2]) continue;
                [vis addObject:k2];
                [q addObject:@[@(px), @(py), @(steps + 1)]];
            }
        }
    }
    return -1;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<NSArray<NSString *>*> *maze = readMaze(@"input.txt");
        NSMutableDictionary *portals, *portalPos;
        findPortals(maze, &portals, &portalPos);
        if (!portals[@"AA"] || !portals[@"ZZ"]) return 1;
        NSArray *start = portals[@"AA"][0], *end = portals[@"ZZ"][0];
        long ans = bfs(maze, portals, portalPos, start, end);
        printf("%ld\n", ans);
    }
    return 0;
}
