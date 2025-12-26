
#import <Foundation/Foundation.h>

static inline NSNumber *keyFor(int x, int y) {
    uint64_t v = ((uint64_t)(int32_t)x << 32) | (uint32_t)y;
    return [NSNumber numberWithUnsignedLongLong:v];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByString:@"\n"];
        NSMutableSet<NSNumber *> *grid = [NSMutableSet set];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray<NSString *> *parts = [line componentsSeparatedByString:@" -> "];
            NSMutableArray<NSArray<NSNumber *> *> *pts = [NSMutableArray array];
            for (NSString *p in parts) {
                NSArray<NSString *> *c = [p componentsSeparatedByString:@","];
                int x = c[0].intValue, y = c[1].intValue;
                [pts addObject:@[@(x), @(y)]];
            }
            for (NSUInteger i = 0; i + 1 < pts.count; ++i) {
                int x1 = pts[i][0].intValue, y1 = pts[i][1].intValue;
                int x2 = pts[i+1][0].intValue, y2 = pts[i+1][1].intValue;
                if (x1 == x2) {
                    int yStart = MIN(y1, y2), yEnd = MAX(y1, y2);
                    for (int y = yStart; y <= yEnd; ++y) {
                        [grid addObject:keyFor(x1, y)];
                    }
                } else {
                    int xStart = MIN(x1, x2), xEnd = MAX(x1, x2);
                    for (int x = xStart; x <= xEnd; ++x) {
                        [grid addObject:keyFor(x, y1)];
                    }
                }
            }
        }

        int floor = 0;
        for (NSNumber *n in grid) {
            uint64_t v = n.unsignedLongLongValue;
            int y = (int32_t)v;
            if (y > floor) floor = y;
        }
        floor += 1;

        int sands = 0, firstFloorTouch = 0;
        BOOL full = NO;
        while (!full) {
            int sx = 500, sy = 0;
            BOOL settled = NO;
            while (!settled) {
                if (sy == floor) {
                    if (firstFloorTouch == 0) firstFloorTouch = sands;
                    [grid addObject:keyFor(sx, sy)];
                    settled = YES;
                    break;
                }
                int nx[3] = {sx, sx-1, sx+1};
                int ny = sy + 1;
                BOOL moved = NO;
                for (int k = 0; k < 3; ++k) {
                    if (![grid containsObject:keyFor(nx[k], ny)]) {
                        sx = nx[k];
                        sy = ny;
                        moved = YES;
                        break;
                    }
                }
                if (!moved) {
                    [grid addObject:keyFor(sx, sy)];
                    settled = YES;
                }
            }
            sands++;
            full = [grid containsObject:keyFor(500, 0)];
        }
        printf("%d\n", firstFloorTouch);
    }
    return 0;
}
