
#import <Foundation/Foundation.h>

typedef struct { int x, y; } Pt;

static inline uint64_t ptKey(Pt p) {
    return ((uint64_t)(uint32_t)p.x << 32) | (uint32_t)p.y;
}

static int cmpPt(const void *a, const void *b) {
    const Pt *pa = (const Pt *)a, *pb = (const Pt *)b;
    if (pa->y != pb->y) return (pa->y < pb->y) ? -1 : 1;
    if (pa->x != pb->x) return (pa->x < pb->x) ? -1 : 1;
    return 0;
}

@interface Orientation : NSObject
@property(nonatomic) Pt *pts;
@property(nonatomic) int n;
@property(nonatomic) int w;
@property(nonatomic) int h;
@end
@implementation Orientation
- (void)dealloc { if (_pts) free(_pts); }
@end

@interface Shape : NSObject
@property(nonatomic,strong) NSArray<Orientation*> *oris;
@property(nonatomic) int area;
@end

static inline Pt transform(Pt p, int i) {
    Pt r;
    switch (i) {
        case 0: r.x = p.x;  r.y = p.y;  break;
        case 1: r.x = p.y;  r.y = -p.x; break;
        case 2: r.x = -p.x; r.y = -p.y; break;
        case 3: r.x = -p.y; r.y = p.x;  break;
        case 4: r.x = -p.x; r.y = p.y;  break;
        case 5: r.x = p.y;  r.y = p.x;  break;
        case 6: r.x = p.x;  r.y = -p.y; break;
        case 7: r.x = -p.y; r.y = -p.x; break;
        default: r.x = p.x; r.y = p.y; break;
    }
    return r;
}

@implementation Shape

- (instancetype)initWithRows:(NSArray<NSString*>*)rows {
    if (!(self = [super init])) return nil;

    NSMutableArray<NSValue*> *pts = [NSMutableArray array];
    for (NSInteger r = 0; r < (NSInteger)rows.count; r++) {
        NSString *row = rows[r];
        NSUInteger len = row.length;
        for (NSUInteger c = 0; c < len; c++) {
            unichar ch = [row characterAtIndex:c];
            if (ch == '#') {
                Pt p; p.x = (int)c; p.y = (int)r;
                [pts addObject:[NSValue valueWithBytes:&p objCType:@encode(Pt)]];
            }
        }
    }
    _area = (int)pts.count;

    int n = (int)pts.count;
    Pt *base = (Pt *)malloc(sizeof(Pt) * n);
    for (int i = 0; i < n; i++) {
        Pt p; [pts[i] getValue:&p];
        base[i] = p;
    }

    NSMutableArray<Orientation*> *res = [NSMutableArray array];
    NSMutableSet<NSData*> *seen = [NSMutableSet setWithCapacity:8];

    Pt *tmp = (Pt *)malloc(sizeof(Pt) * n);

    for (int t = 0; t < 8; t++) {
        int minX = INT_MAX, minY = INT_MAX;
        for (int i = 0; i < n; i++) {
            Pt p = transform(base[i], t);
            tmp[i] = p;
            if (p.x < minX) minX = p.x;
            if (p.y < minY) minY = p.y;
        }
        for (int i = 0; i < n; i++) {
            tmp[i].x -= minX;
            tmp[i].y -= minY;
        }
        qsort(tmp, n, sizeof(Pt), cmpPt);

        NSData *sig = [NSData dataWithBytes:tmp length:sizeof(Pt)*n];
        if ([seen containsObject:sig]) continue;
        [seen addObject:sig];

        Orientation *o = [Orientation new];
        o.n = n;
        o.pts = (Pt *)malloc(sizeof(Pt) * n);
        memcpy(o.pts, tmp, sizeof(Pt) * n);

        int mw = 0, mh = 0;
        for (int i = 0; i < n; i++) {
            if (o.pts[i].x > mw) mw = o.pts[i].x;
            if (o.pts[i].y > mh) mh = o.pts[i].y;
        }
        o.w = mw + 1;
        o.h = mh + 1;
        [res addObject:o];
    }

    free(tmp);
    free(base);

    _oris = res;
    return self;
}

@end

static int totalPossible = 0;

static BOOL solve(int idx, uint8_t *grid, int W, int H, NSArray<Shape*> *toFit, int remArea, int freeArea) {
    if (idx == (int)toFit.count) return YES;
    if (remArea > freeArea) return NO;

    Shape *shape = toFit[idx];
    for (Orientation *o in shape.oris) {
        if (o.w > W || o.h > H) continue;

        for (int r = 0; r <= H - o.h; r++) {
            for (int c = 0; c <= W - o.w; c++) {
                BOOL ok = YES;
                Pt *pts = o.pts;
                int n = o.n;

                for (int i = 0; i < n; i++) {
                    int rr = r + pts[i].y;
                    int cc = c + pts[i].x;
                    if (grid[rr * W + cc]) { ok = NO; break; }
                }
                if (!ok) continue;

                for (int i = 0; i < n; i++) {
                    int rr = r + pts[i].y;
                    int cc = c + pts[i].x;
                    grid[rr * W + cc] = 1;
                }

                if (solve(idx + 1, grid, W, H, toFit, remArea - shape.area, freeArea - shape.area)) return YES;

                for (int i = 0; i < n; i++) {
                    int rr = r + pts[i].y;
                    int cc = c + pts[i].x;
                    grid[rr * W + cc] = 0;
                }
            }
        }
    }
    return NO;
}

static void handleRegion(NSString *line, NSArray<Shape*> *allShapes) {
    NSArray<NSString*> *parts = [line componentsSeparatedByString:@":"];
    if (parts.count < 2) return;

    NSString *dimPart = [parts[0] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    NSArray<NSString*> *dims = [dimPart componentsSeparatedByString:@"x"];
    if (dims.count < 2) return;

    int W = (int)[dims[0] intValue];
    int H = (int)[dims[1] intValue];
    if (W <= 0 || H <= 0) return;

    NSString *countsPart = [parts[1] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    NSArray<NSString*> *counts = [countsPart componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
    NSMutableArray<Shape*> *toFit = [NSMutableArray array];
    int totalRequiredArea = 0;

    NSInteger si = 0;
    for (NSString *tok in counts) {
        if (tok.length == 0) continue;
        if (si >= (NSInteger)allShapes.count) break;
        int qty = (int)[tok intValue];
        for (int q = 0; q < qty; q++) {
            Shape *s = allShapes[si];
            [toFit addObject:s];
            totalRequiredArea += s.area;
        }
        si++;
    }

    [toFit sortUsingComparator:^NSComparisonResult(Shape *a, Shape *b) {
        if (a.area == b.area) return NSOrderedSame;
        return (a.area > b.area) ? NSOrderedAscending : NSOrderedDescending;
    }];

    int cells = W * H;
    if (totalRequiredArea > cells) return;

    uint8_t *grid = (uint8_t *)calloc((size_t)cells, 1);
    BOOL ok = solve(0, grid, W, H, toFit, totalRequiredArea, cells);
    free(grid);
    if (ok) totalPossible++;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSData *data = [NSData dataWithContentsOfFile:path];
        if (!data) return 0;

        NSString *content = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        if (!content) return 0;

        NSArray<NSString*> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<Shape*> *allShapes = [NSMutableArray array];

        NSInteger i = 0;
        while (i < (NSInteger)lines.count) {
            NSString *line = lines[i];
            NSString *trim = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (trim.length == 0) { i++; continue; }

            if ([trim containsString:@"x"] && [trim containsString:@":"] && ![trim hasSuffix:@":"]) {
                handleRegion(trim, allShapes);
                i++;
                continue;
            }

            if ([trim hasSuffix:@":"]) {
                NSMutableArray<NSString*> *rows = [NSMutableArray array];
                i++;
                while (i < (NSInteger)lines.count) {
                    NSString *ln = lines[i];
                    NSString *t2 = [ln stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
                    if (t2.length == 0) { i++; break; }
                    if ([t2 containsString:@":"]) break;

                    NSRange rHash = [t2 rangeOfString:@"#"];
                    NSRange rDot  = [t2 rangeOfString:@"."];
                    if (rHash.location != NSNotFound || rDot.location != NSNotFound) {
                        [rows addObject:t2];
                        i++;
                    } else {
                        break;
                    }
                }
                if (rows.count > 0) [allShapes addObject:[[Shape alloc] initWithRows:rows]];
                continue;
            }

            i++;
        }

        printf("%d\n", totalPossible);
    }
    return 0;
}
