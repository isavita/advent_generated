
#import <Foundation/Foundation.h>

typedef struct { int x, y; } P;

BOOL can(int y, int x, int dy, int dx, int H, int W, char m[H][W]) {
    char c = m[y][x];
    if (c == '.') return YES;
    if (c == '#') return NO;
    if (c == '[') return (dy == 0) ? can(y, x + 2 * dx, dy, dx, H, W, m) : (can(y + dy, x, dy, dx, H, W, m) && can(y + dy, x + 1, dy, dx, H, W, m));
    if (c == ']') return (dy == 0) ? can(y, x + 2 * dx, dy, dx, H, W, m) : (can(y + dy, x, dy, dx, H, W, m) && can(y + dy, x - 1, dy, dx, H, W, m));
    return can(y + dy, x + dx, dy, dx, H, W, m);
}

void mov(int y, int x, int dy, int dx, int H, int W, char m[H][W]) {
    char c = m[y][x];
    if (c == '.' || c == '#') return;
    if (dy == 0 || c == 'O' || c == '@') {
        mov(y + dy, x + dx, dy, dx, H, W, m);
        m[y + dy][x + dx] = c; m[y][x] = '.';
    } else {
        int x2 = (c == '[') ? x + 1 : x - 1;
        char c2 = m[y][x2];
        mov(y + dy, x, dy, dx, H, W, m);
        mov(y + dy, x2, dy, dx, H, W, m);
        m[y + dy][x] = c; m[y + dy][x2] = c2;
        m[y][x] = '.'; m[y][x2] = '.';
    }
}

long long solve(NSString *gridStr, NSString *moveStr, BOOL p2) {
    NSArray *lines = [gridStr componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
    NSMutableArray *clean = [NSMutableArray new];
    for (NSString *s in lines) if (s.length > 0) [clean addObject:s];
    int H = (int)clean.count, W = (int)[clean[0] length] * (p2 ? 2 : 1);
    char m[H][W];
    int ry = 0, rx = 0;
    for (int y = 0; y < H; y++) {
        NSString *l = clean[y];
        for (int x = 0; x < l.length; x++) {
            char c = [l characterAtIndex:x];
            if (!p2) { m[y][x] = c; if (c == '@') { ry = y; rx = x; } }
            else {
                if (c == '#') { m[y][2*x] = '#'; m[y][2*x+1] = '#'; }
                else if (c == 'O') { m[y][2*x] = '['; m[y][2*x+1] = ']'; }
                else if (c == '.') { m[y][2*x] = '.'; m[y][2*x+1] = '.'; }
                else if (c == '@') { m[y][2*x] = '@'; m[y][2*x+1] = '.'; ry = y; rx = 2*x; }
            }
        }
    }
    for (NSUInteger i = 0; i < moveStr.length; i++) {
        char v = [moveStr characterAtIndex:i];
        int dy = (v == '^') ? -1 : (v == 'v' ? 1 : 0), dx = (v == '<') ? -1 : (v == '>' ? 1 : 0);
        if (dy || dx) if (can(ry + dy, rx + dx, dy, dx, H, W, m)) {
            mov(ry + dy, rx + dx, dy, dx, H, W, m);
            m[ry][rx] = '.'; ry += dy; rx += dx; m[ry][rx] = '@';
        }
    }
    long long s = 0;
    for (int y = 0; y < H; y++) for (int x = 0; x < W; x++) if (m[y][x] == 'O' || m[y][x] == '[') s += 100 * y + x;
    return s;
}

int main() {
    @autoreleasepool {
        NSString *in = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!in) return 0;
        NSArray *p = [in componentsSeparatedByString:@"\n\n"];
        if (p.count >= 2) printf("%lld\n%lld\n", solve(p[0], p[1], NO), solve(p[0], p[1], YES));
    }
    return 0;
}

