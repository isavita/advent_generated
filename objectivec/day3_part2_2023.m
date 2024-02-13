#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *grid = [NSMutableDictionary dictionary];
        NSMutableArray *parts = [NSMutableArray array];
        NSMutableDictionary *partsGrid = [NSMutableDictionary dictionary];
        
        NSArray *neighbors8 = @[@[@0, @1], @[@0, @-1], @[@1, @0], @[@-1, @0],
                                @[@-1, @-1], @[@-1, @1], @[@1, @-1], @[@1, @1]];
        
        for (int y = 0; y < [lines count]; y++) {
            NSString *line = lines[y];
            int xmin = 0;
            int xmax = 0;
            int n = 0;
            int currY = y;
            for (int x = 0; x < [line length]; x++) {
                unichar c = [line characterAtIndex:x];
                grid[[NSValue valueWithPoint:NSMakePoint(x, y)]] = [NSString stringWithFormat:@"%C", c];
                if (c >= '0' && c <= '9') {
                    if (n == 0) {
                        xmin = x;
                        xmax = x;
                        n = c - '0';
                    } else {
                        n *= 10;
                        n += c - '0';
                        xmax = x;
                    }
                } else if (n != 0) {
                    [parts addObject:@{@"xmin":@(xmin), @"xmax":@(xmax), @"y":@(currY), @"n":@(n)}];
                    for (int i = xmin; i <= xmax; i++) {
                        partsGrid[[NSValue valueWithPoint:NSMakePoint(i, currY)]] = @([parts count] - 1);
                    }
                    n = 0;
                }
            }
            if (n != 0) {
                [parts addObject:@{@"xmin":@(xmin), @"xmax":@(xmax), @"y":@(currY), @"n":@(n)}];
                for (int i = xmin; i <= xmax; i++) {
                    partsGrid[[NSValue valueWithPoint:NSMakePoint(i, currY)]] = @([parts count] - 1);
                }
            }
        }
        
        int sum = 0;
        for (NSValue *p in [grid allKeys]) {
            NSPoint point = [p pointValue];
            NSString *c = grid[p];
            if ([c isEqualToString:@"*"]) {
                NSMutableSet *neighborParts = [NSMutableSet set];
                for (NSArray *n in neighbors8) {
                    NSPoint neighbor = NSMakePoint(point.x + [n[0] intValue], point.y + [n[1] intValue]);
                    NSNumber *i = partsGrid[[NSValue valueWithPoint:neighbor]];
                    if (i != nil) {
                        [neighborParts addObject:i];
                    }
                }
                if ([neighborParts count] == 2) {
                    int prod = 1;
                    for (NSNumber *i in neighborParts) {
                        NSDictionary *part = parts[[i intValue]];
                        prod *= [part[@"n"] intValue];
                    }
                    sum += prod;
                }
            }
        }
        
        printf("%d\n", sum);
    }
    return 0;
}