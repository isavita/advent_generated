#import <Foundation/Foundation.h>

@interface Node : NSObject
@property (assign) NSInteger x;
@property (assign) NSInteger y;
@property (assign) NSInteger dist;
- (instancetype)initWithX:(NSInteger)x y:(NSInteger)y dist:(NSInteger)dist;
@end
@implementation Node
- (instancetype)initWithX:(NSInteger)x y:(NSInteger)y dist:(NSInteger)dist {
    self = [super init];
    if (self) { self.x = x; self.y = y; self.dist = dist; }
    return self;
}
@end

static NSString * const KEY(NSString *k) { return k; }

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        NSMutableDictionary *grid = [NSMutableDictionary dictionary];
        NSMutableArray *as = [NSMutableArray array];
        NSInteger y = 0, sx = 0, sy = 0, ex = 0, ey = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            for (NSInteger x = 0; x < line.length; x++) {
                unichar c = [line characterAtIndex:x];
                NSString *key = KEY([NSString stringWithFormat:@"%ld,%ld", x, y]);
                grid[key] = @(c);
                if (c == 'S') { sx = x; sy = y; }
                else if (c == 'E') { ex = x; ey = y; }
                else if (c == 'a') [as addObject:key];
            }
            y++;
        }
        NSString *sKey = KEY([NSString stringWithFormat:@"%ld,%ld", sx, sy]);
        NSString *eKey = KEY([NSString stringWithFormat:@"%ld,%ld", ex, ey]);
        grid[sKey] = @('a');
        grid[eKey] = @('z');

        NSMutableDictionary *dist = [NSMutableDictionary dictionary];
        NSComparator cmp = ^NSComparisonResult(id _a, id _b) {
            Node *a = _a, *b = _b;
            if (a.dist < b.dist) return NSOrderedAscending;
            if (a.dist > b.dist) return NSOrderedDescending;
            return NSOrderedSame;
        };
        NSMutableArray *pq = [NSMutableArray array];
        Node *start = [[Node alloc] initWithX:ex y:ey dist:0];
        [pq addObject:start];
        dist[eKey] = @0;

        while (pq.count) {
            [pq sortUsingComparator:cmp];
            Node *curr = pq.firstObject;
            [pq removeObjectAtIndex:0];
            NSInteger cx = curr.x, cy = curr.y;
            NSArray *dirs = @[@0, @1, @0, @(-1), @1, @0, @(-1), @0];
            for (NSInteger i = 0; i < dirs.count; i += 2) {
                NSInteger nx = cx + [dirs[i] integerValue];
                NSInteger ny = cy + [dirs[i+1] integerValue];
                NSString *nk = KEY([NSString stringWithFormat:@"%ld,%ld", nx, ny]);
                if (!grid[nk]) continue;
                unichar cv = [grid[KEY([NSString stringWithFormat:@"%ld,%ld", cx, cy])] unsignedCharValue];
                unichar nv = [grid[nk] unsignedCharValue];
                if (cv - nv > 1) continue;
                NSInteger nd = curr.dist + 1;
                NSNumber *od = dist[nk];
                if (!od || nd < [od integerValue]) {
                    dist[nk] = @(nd);
                    [pq addObject:[[Node alloc] initWithX:nx y:ny dist:nd]];
                }
            }
        }

        NSInteger best = [dist[sKey] integerValue];
        for (NSString *ak in as) {
            NSNumber *d = dist[ak];
            if (d && [d integerValue] < best) best = [d integerValue];
        }
        printf("%ld\n", best);
    }
    return 0;
}