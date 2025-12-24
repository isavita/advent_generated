#import <Foundation/Foundation.h>

@interface Coord : NSObject
@property (nonatomic) int x, y;
- (instancetype)initWithX:(int)x y:(int)y;
- (BOOL)isEqual:(id)object;
- (NSUInteger)hash;
@end

@implementation Coord
- (instancetype)initWithX:(int)x y:(int)y {
    self = [super init];
    if (self) { _x = x; _y = y; }
    return self;
}
- (BOOL)isEqual:(id)object {
    Coord *other = (Coord *)object;
    return self.x == other.x && self.y == other.y;
}
- (NSUInteger)hash {
    return (@(self.x).hash ^ (@(self.y).hash + 0x9e3779b9 + (self.x << 6) + (self.x >> 2)));
}
@end

static NSArray<NSString *> *readFile(NSString *name) {
    NSError *err = nil;
    NSString *s = [NSString stringWithContentsOfFile:name encoding:NSUTF8StringEncoding error:&err];
    if (err) { NSLog(@"Error: %@", err); exit(1); }
    return [s componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
}

static long long solve(NSArray<NSString *> *lines, int factor) {
    if (!lines.count) return 0;
    int W = (int)lines[0].length, H = (int)lines.count;
    NSMutableSet<Coord *> *gals = [NSMutableSet set];
    for (int y = 0; y < H; y++) {
        NSString *row = lines[y];
        for (int x = 0; x < W; x++)
            if ([row characterAtIndex:x] != '.')
                [gals addObject:[[Coord alloc] initWithX:x y:y]];
    }
    NSMutableIndexSet *emptyR = [NSMutableIndexSet indexSet], *emptyC = [NSMutableIndexSet indexSet];
    for (int y = 0; y < H; y++) {
        __block BOOL ok = YES;
        [gals enumerateObjectsUsingBlock:^(Coord *c, BOOL *stop) { if (c.y == y) { ok = NO; *stop = YES; } }];
        if (ok) [emptyR addIndex:y];
    }
    for (int x = 0; x < W; x++) {
        __block BOOL ok = YES;
        [gals enumerateObjectsUsingBlock:^(Coord *c, BOOL *stop) { if (c.x == x) { ok = NO; *stop = YES; } }];
        if (ok) [emptyC addIndex:x];
    }
    long long add = factor - 1, total = 0;
    NSArray<Coord *> *arr = [gals allObjects];
    for (NSUInteger i = 0; i < arr.count; i++) {
        for (NSUInteger j = i + 1; j < arr.count; j++) {
            Coord *a = arr[i], *b = arr[j];
            long long x1 = a.x + add * [emptyC countOfIndexesInRange:NSMakeRange(0, a.x)],
                      x2 = b.x + add * [emptyC countOfIndexesInRange:NSMakeRange(0, b.x)],
                      y1 = a.y + add * [emptyR countOfIndexesInRange:NSMakeRange(0, a.y)],
                      y2 = b.y + add * [emptyR countOfIndexesInRange:NSMakeRange(0, b.y)];
            total += llabs(x2 - x1) + llabs(y2 - y1);
        }
    }
    return total;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<NSString *> *lines = readFile(@"input.txt");
        printf("%lld\n", solve(lines, 1000000));
    }
    return 0;
}