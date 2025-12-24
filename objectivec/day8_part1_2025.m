
#import <Foundation/Foundation.h>

@interface UnionFind : NSObject {
    NSMutableArray *parent;
    NSMutableArray *sz;
}
- (instancetype)initWithN:(int)n;
- (int)find:(int)x;
- (void)unite:(int)a with:(int)b;
- (int)rootSize:(int)x;
@end

@implementation UnionFind
- (instancetype)initWithN:(int)n {
    self = [super init];
    if (self) {
        parent = [NSMutableArray arrayWithCapacity:n];
        sz = [NSMutableArray arrayWithCapacity:n];
        for (int i = 0; i < n; i++) {
            [parent addObject:@(i)];
            [sz addObject:@(1)];
        }
    }
    return self;
}
- (int)find:(int)x {
    int px = [parent[x] intValue];
    if (px == x) return x;
    int rx = [self find:px];
    parent[x] = @(rx);
    return rx;
}
- (void)unite:(int)a with:(int)b {
    int ra = [self find:a];
    int rb = [self find:b];
    if (ra == rb) return;
    int sza = [sz[ra] intValue];
    int szb = [sz[rb] intValue];
    if (sza < szb) {
        int tmp = ra; ra = rb; rb = tmp;
    }
    parent[rb] = @(ra);
    sz[ra] = @(sza + szb);
}
- (int)rootSize:(int)x {
    int r = [self find:x];
    return [sz[r] intValue];
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *whole = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if (!whole) return 1;

        NSArray *lines = [whole componentsSeparatedByString:@"\n"];
        NSMutableArray *pts = [NSMutableArray array];
        for (NSString *l in lines) {
            int x, y, z;
            if (sscanf([l UTF8String], " %d , %d , %d", &x, &y, &z) == 3) {
                [pts addObject:@[@(x), @(y), @(z)]];
            }
        }

        int n = (int)pts.count;
        if (n < 2) {
            printf("Not enough points to form circuits.\n");
            return 0;
        }

        NSMutableArray *edges = [NSMutableArray array];
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                long long dx = [pts[i][0] longLongValue] - [pts[j][0] longLongValue];
                long long dy = [pts[i][1] longLongValue] - [pts[j][1] longLongValue];
                long long dz = [pts[i][2] longLongValue] - [pts[j][2] longLongValue];
                long long d = dx*dx + dy*dy + dz*dz;
                [edges addObject:@[@(i), @(j), @(d)]];
            }
        }

        [edges sortUsingComparator:^NSComparisonResult(NSArray *a, NSArray *b) {
            long long da = [a[2] longLongValue];
            long long db = [b[2] longLongValue];
            return (da < db) ? NSOrderedAscending : (da > db) ? NSOrderedDescending : NSOrderedSame;
        }];

        UnionFind *uf = [[UnionFind alloc] initWithN:n];
        NSUInteger limit = MIN(1000U, edges.count);
        for (NSUInteger i = 0; i < limit; i++) {
            int u = [edges[i][0] intValue];
            int v = [edges[i][1] intValue];
            [uf unite:u with:v];
        }

        int top[3] = {0, 0, 0};
        for (int i = 0; i < n; i++) {
            if ([uf find:i] == i) {
                int s = [uf rootSize:i];
                if (s > top[0]) {
                    top[2] = top[1]; top[1] = top[0]; top[0] = s;
                } else if (s > top[1]) {
                    top[2] = top[1]; top[1] = s;
                } else if (s > top[2]) {
                    top[2] = s;
                }
            }
        }

        unsigned long long product = 1;
        for (int k = 0; k < 3 && top[k] > 0; k++) product *= (unsigned long long)top[k];
        printf("Product of three largest circuit sizes: %llu\n", product);
    }
    return 0;
}
