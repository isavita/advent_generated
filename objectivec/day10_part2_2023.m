
#import <Foundation/Foundation.h>

typedef struct { int x, y; } Coord;

static const Coord DirUp = {0, -1};
static const Coord DirRight = {1, 0};
static const Coord DirDown = {0, 1};
static const Coord DirLeft = {-1, 0};

static inline Coord add(Coord a, Coord b) { return (Coord){a.x + b.x, a.y + b.y}; }
static inline Coord opposite(Coord d) { return (Coord){-d.x, -d.y}; }

static inline NSString *key(Coord c) { return [NSString stringWithFormat:@"%d,%d", c.x, c.y]; }

static const int MaskUp = 1, MaskRight = 2, MaskDown = 4, MaskLeft = 8;
static const int MaskUndefined = 0;

static int maskFromTile(unichar t) {
    switch (t) {
        case '|': return MaskUp | MaskDown;
        case '-': return MaskLeft | MaskRight;
        case 'J': return MaskUp | MaskLeft;
        case 'L': return MaskUp | MaskRight;
        case '7': return MaskDown | MaskLeft;
        case 'F': return MaskDown | MaskRight;
        default:  return 0;
    }
}
static unichar tileFromMask(int m) {
    switch (m) {
        case MaskUp | MaskDown: return '|';
        case MaskLeft | MaskRight: return '-';
        case MaskUp | MaskLeft: return 'J';
        case MaskUp | MaskRight: return 'L';
        case MaskDown | MaskLeft: return '7';
        case MaskDown | MaskRight: return 'F';
        default: return '.';
    }
}
static int dirToMask(Coord d) {
    if (d.x == 0 && d.y == -1) return MaskUp;
    if (d.x == 1 && d.y == 0)  return MaskRight;
    if (d.x == 0 && d.y == 1)  return MaskDown;
    if (d.x == -1 && d.y == 0) return MaskLeft;
    return 0;
}
static Coord maskToDir(int m) {
    if (m == MaskUp) return DirUp;
    if (m == MaskRight) return DirRight;
    if (m == MaskDown) return DirDown;
    if (m == MaskLeft) return DirLeft;
    return (Coord){0,0};
}

static int pipeFromNeighbors(Coord c, NSDictionary<NSString*, NSString*> *grid) {
    int pipe = 0;
    Coord dirs[4] = {DirUp, DirRight, DirDown, DirLeft};
    for (int i = 0; i < 4; ++i) {
        Coord n = add(c, dirs[i]);
        NSString *v = grid[key(n)];
        if (v) {
            int neighborMask = maskFromTile([v characterAtIndex:0]);
            if (neighborMask & dirToMask(opposite(dirs[i]))) pipe |= dirToMask(dirs[i]);
        }
    }
    return pipe;
}

static NSArray<NSValue*>* pathFinding(Coord start, NSDictionary<NSString*, NSString*> *grid) {
    NSMutableArray<NSValue*> *path = [NSMutableArray array];
    [path addObject:[NSValue valueWithBytes:&start objCType:@encode(Coord)]];
    int startPipe = pipeFromNeighbors(start, grid);
    Coord prevDir = DirUp; // placeholder
    Coord cur = start;
    for (int m = MaskUp; m <= MaskLeft; m <<= 1) {
        if (startPipe & m) { prevDir = maskToDir(m); cur = add(start, prevDir); break; }
    }
    while (cur.x != start.x || cur.y != start.y) {
        [path addObject:[NSValue valueWithBytes:&cur objCType:@encode(Coord)]];
        NSString *ch = grid[key(cur)];
        int curMask = maskFromTile([ch characterAtIndex:0]);
        for (int m = MaskUp; m <= MaskLeft; m <<= 1) {
            if (curMask & m) {
                Coord d = maskToDir(m);
                if (d.x != -prevDir.x || d.y != -prevDir.y) {
                    prevDir = d;
                    cur = add(cur, d);
                    break;
                }
            }
        }
    }
    return path;
}

static NSMutableDictionary<NSString*, NSString*>* pathGrid(NSDictionary<NSString*, NSString*> *grid,
                                                          NSArray<NSValue*> *path) {
    NSMutableDictionary<NSString*, NSString*> *pg = [NSMutableDictionary dictionary];
    for (NSValue *v in path) {
        Coord c; [v getValue:&c];
        pg[key(c)] = grid[key(c)];
    }
    Coord start; [path[0] getValue:&start];
    int startPipe = pipeFromNeighbors(start, grid);
    pg[key(start)] = [NSString stringWithFormat:@"%c", tileFromMask(startPipe)];
    return pg;
}

static BOOL isInside(Coord c, NSDictionary<NSString*, NSString*> *pg) {
    if (pg[key(c)]) return NO;
    int leftPipes = 0;
    int pending = 0;
    for (int x = 0; x < c.x; ++x) {
        NSString *ch = pg[key((Coord){x, c.y})];
        if (!ch) continue;
        unichar t = [ch characterAtIndex:0];
        if (t == '|') leftPipes++;
        else if (t == 'L') pending = MaskUp | MaskRight;
        else if (t == 'F') pending = MaskDown | MaskRight;
        else if (t == 'J') {
            if (pending == (MaskDown | MaskRight)) { leftPipes++; pending = 0; }
            else if (pending == (MaskUp | MaskRight)) pending = 0;
        } else if (t == '7') {
            if (pending == (MaskUp | MaskRight)) { leftPipes++; pending = 0; }
            else if (pending == (MaskDown | MaskRight)) pending = 0;
        }
    }
    return (leftPipes % 2) == 1;
}

static int solve(NSArray<NSString*> *lines) {
    NSMutableDictionary<NSString*, NSString*> *grid = [NSMutableDictionary dictionary];
    for (int y = 0; y < lines.count; ++y) {
        NSString *line = lines[y];
        for (int x = 0; x < line.length; ++x) {
            unichar ch = [line characterAtIndex:x];
            if (ch != '.') grid[key((Coord){x, y})] = [NSString stringWithFormat:@"%c", ch];
        }
    }
    Coord start = (Coord){0,0};
    for (NSString *k in grid) {
        if ([grid[k] isEqualToString:@"S"]) {
            NSArray *parts = [k componentsSeparatedByString:@","];
            start.x = [parts[0] intValue];
            start.y = [parts[1] intValue];
            break;
        }
    }
    NSArray<NSValue*> *path = pathFinding(start, grid);
    NSDictionary<NSString*, NSString*> *pg = pathGrid(grid, path);
    int cnt = 0;
    int maxY = lines.count;
    int maxX = lines.firstObject.length;
    for (int y = 0; y < maxY; ++y) {
        for (int x = 0; x < maxX; ++x) {
            if (isInside((Coord){x, y}, pg)) cnt++;
        }
    }
    return cnt;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *text = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray<NSString*> *lines = [text componentsSeparatedByCharactersInSet:
                                     [NSCharacterSet newlineCharacterSet]];
        if (lines.lastObject.length == 0) lines = [lines subarrayWithRange:NSMakeRange(0, lines.count-1)];
        int ans = solve(lines);
        printf("%d\n", ans);
    }
    return 0;
}
