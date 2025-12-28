
#import <Foundation/Foundation.h>

static NSCharacterSet *nonRock;
static int W, H, SX, SY;

static int mod(int v, int m) { int r = v % m; return r < 0 ? r + m : r; }

static uint64_t solve(void) {
    NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                               encoding:NSUTF8StringEncoding
                                                  error:nil];
    NSArray<NSString *> *lines = [txt componentsSeparatedByString:@"\n"];
    while ([lines.lastObject length] == 0) lines = [lines subarrayWithRange:NSMakeRange(0, lines.count-1)];
    H = (int)lines.count;
    W = (int)lines[0].length;
    nonRock = [NSMutableCharacterSet characterSetWithCharactersInString:@".S"];
    for (int y = 0; y < H; ++y) {
        NSString *row = lines[y];
        for (int x = 0; x < W; ++x)
            if ([row characterAtIndex:x] == 'S') { SX = x; SY = y; break; }
    }

    int steps = 26501365;
    int64_t a = 0, b = 0, c = 0;
    NSMutableSet<NSString *> *q = [NSMutableSet setWithObject:[NSString stringWithFormat:@"%d,%d", SX, SY]];
    for (int t = 1; t <= 3*W; ++t) {
        NSMutableSet<NSString *> *nq = [NSMutableSet set];
        for (NSString *key in q) {
            NSArray *p = [key componentsSeparatedByString:@","];
            int x = [p[0] intValue], y = [p[1] intValue];
            for (int dy = -1; dy <= 1; ++dy)
                for (int dx = -1; dx <= 1; ++dx)
                    if (dx*dx + dy*dy == 1) {
                        int nx = x + dx, ny = y + dy;
                        int rx = mod(nx, W), ry = mod(ny, H);
                        unichar ch = [lines[ry] characterAtIndex:rx];
                        if ([nonRock characterIsMember:ch])
                            [nq addObject:[NSString stringWithFormat:@"%d,%d", nx, ny]];
                    }
        }
        q = nq;
        if (t % W == (W/2)) {
            if (a == 0) a = q.count;
            else if (b == 0) b = q.count;
            else { c = q.count; break; }
        }
    }
    int64_t n = steps / W;
    return a + n*(b - a) + n*(n - 1)/2*(c - 2*b + a);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        printf("%llu\n", solve());
    }
    return 0;
}
