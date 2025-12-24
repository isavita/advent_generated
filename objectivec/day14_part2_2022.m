#import <Foundation/Foundation.h>

#define MAXR 200
#define MAXC 1000
#define PAD 250
#define START 500

char grid[MAXR][MAXC];
int minC = INT_MAX, maxR = 0, maxC = 0;

static inline int min(int a, int b) { return a < b ? a : b; }
static inline int max(int a, int b) { return a > b ? a : b; }

void draw(int r1, int c1, int r2, int c2) {
    c1 = c1 - minC + PAD;
    c2 = c2 - minC + PAD;
    if (r1 == r2) {
        for (int c = min(c1, c2); c <= max(c1, c2); c++) grid[r1][c] = '#';
    } else {
        for (int r = min(r1, r2); r <= max(r1, r2); r++) grid[r][c1] = '#';
    }
}

int drop(int c0) {
    int r = 0, c = c0;
    if (grid[r][c] == 'o') return 0;
    while (r < MAXR - 1) {
        if (grid[r + 1][c] == '.') r++;
        else if (c > 0 && grid[r + 1][c - 1] == '.') { r++; c--; }
        else if (c < MAXC - 1 && grid[r + 1][c + 1] == '.') { r++; c++; }
        else { grid[r][c] = 'o'; return 1; }
    }
    return 0;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        for (int i = 0; i < MAXR; i++) memset(grid[i], '.', MAXC);

        // bounds pass
        for (NSString *line in [txt componentsSeparatedByString:@"\n"]) {
            for (NSString *part in [line componentsSeparatedByString:@" -> "]) {
                NSArray *xy = [part componentsSeparatedByString:@","];
                if (xy.count == 2) {
                    int x = [xy[0] intValue], y = [xy[1] intValue];
                    minC = min(minC, x);
                    maxR = max(maxR, y);
                    maxC = max(maxC, x);
                }
            }
        }

        // draw rocks
        for (NSString *line in [txt componentsSeparatedByString:@"\n"]) {
            NSArray *pts = [line componentsSeparatedByString:@" -> "];
            int px = 0, py = 0;
            for (NSUInteger i = 0; i < pts.count; i++) {
                NSArray *xy = [pts[i] componentsSeparatedByString:@","];
                int x = [xy[0] intValue], y = [xy[1] intValue];
                if (i) draw(py, px, y, x);
                px = x; py = y;
            }
        }

        // floor
        int floor = maxR + 2;
        for (int c = 0; c < MAXC; c++) grid[floor][c] = '#';

        int origin = START - minC + PAD, cnt = 0;
        while (drop(origin)) cnt++;
        printf("%d\n", cnt);
    }
    return 0;
}