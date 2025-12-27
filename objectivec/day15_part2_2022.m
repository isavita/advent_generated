#import <Foundation/Foundation.h>

@interface Sensor : NSObject
@property (assign) NSInteger x, y, bx, by, dist;
@end
@implementation Sensor
@end

static NSInteger manhattan(NSInteger x1, NSInteger y1, NSInteger x2, NSInteger y2) {
    return labs(x1 - x2) + labs(y1 - y2);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:&err];
        if (!txt) { NSLog(@"%@", err); return 1; }

        NSMutableArray<Sensor *> *sensors = [NSMutableArray array];
        NSRegularExpression *re = [NSRegularExpression regularExpressionWithPattern:
                                   @"Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
                                                                        options:0
                                                                          error:nil];
        [re enumerateMatchesInString:txt options:0 range:NSMakeRange(0, txt.length)
                            usingBlock:^(NSTextCheckingResult *m, NSMatchingFlags, BOOL *) {
            Sensor *s = [Sensor new];
            s.x = [[txt substringWithRange:[m rangeAtIndex:1]] integerValue];
            s.y = [[txt substringWithRange:[m rangeAtIndex:2]] integerValue];
            s.bx = [[txt substringWithRange:[m rangeAtIndex:3]] integerValue];
            s.by = [[txt substringWithRange:[m rangeAtIndex:4]] integerValue];
            s.dist = manhattan(s.x, s.y, s.bx, s.by);
            [sensors addObject:s];
        }];

        const NSInteger maxc = 4000000;
        for (NSInteger x = 0; x <= maxc; ++x) {
            NSInteger y = 0;
            while (y <= maxc) {
                BOOL covered = NO;
                NSInteger skip = 0;
                for (Sensor *s in sensors) {
                    NSInteger md = manhattan(x, y, s.x, s.y);
                    if (md <= s.dist) {
                        covered = YES;
                        NSInteger dy = s.dist - labs(x - s.x);
                        skip = MAX(skip, dy + s.y - y);
                    }
                }
                if (!covered) {
                    printf("%lld\n", (long long)x * 4000000LL + y);
                    return 0;
                }
                y += MAX(1, skip);
            }
        }
        printf("-1\n");
    }
    return 0;
}