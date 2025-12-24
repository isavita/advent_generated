
#import <Foundation/Foundation.h>

@interface Galaxy : NSObject
@property int x,y;
@end
@implementation Galaxy
@end

static int dist(Galaxy *a, Galaxy *b, NSArray *emptyRows, NSArray *emptyCols, int factor) {
    int dx = abs(a.x - b.x);
    int dy = abs(a.y - b.y);
    int minX = MIN(a.x, b.x), maxX = MAX(a.x, b.x);
    int minY = MIN(a.y, b.y), maxY = MAX(a.y, b.y);
    for (NSNumber *n in emptyCols) {
        int x = n.intValue;
        if (x > minX && x < maxX) dx += factor - 1;
    }
    for (NSNumber *n in emptyRows) {
        int y = n.intValue;
        if (y > minY && y < maxY) dy += factor - 1;
    }
    return dx + dy;
}

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:nil];
        NSArray *lines = [txt componentsSeparatedByCharactersInSet:
                          [NSCharacterSet newlineCharacterSet]];
        NSMutableArray *rows = [NSMutableArray array];
        NSMutableArray *cols = [NSMutableArray array];
        for (NSString *line in lines)
            if (line.length) [rows addObject:line];
        int W = ((NSString *)rows[0]).length, H = (int)rows.count;
        for (int y = 0; y < H; y++) {
            BOOL empty = YES;
            for (int x = 0; x < W; x++)
                if ([rows[y] characterAtIndex:x] == '#') { empty = NO; break; }
            if (empty) [rows addObject:@(y)];
        }
        for (int x = 0; x < W; x++) {
            BOOL empty = YES;
            for (int y = 0; y < H; y++)
                if ([rows[y] characterAtIndex:x] == '#') { empty = NO; break; }
            if (empty) [cols addObject:@(x)];
        }
        NSMutableArray *gals = [NSMutableArray array];
        for (int y = 0; y < H; y++)
            for (int x = 0; x < W; x++)
                if ([rows[y] characterAtIndex:x] == '#') {
                    Galaxy *g = [[Galaxy alloc] init];
                    g.x = x; g.y = y;
                    [gals addObject:g];
                }
        long long sum = 0;
        for (int i = 0; i < gals.count; i++)
            for (int j = i + 1; j < gals.count; j++)
                sum += dist(gals[i], gals[j], rows, cols, 2);
        printf("%lld\n", sum);
    }
    return 0;
}
