
#import <Foundation/Foundation.h>

@interface Range : NSObject
@property (nonatomic) long long min;
@property (nonatomic) long long max;
@end

@implementation Range
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *text = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if (!text) return 1;

        NSArray *lines = [text componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray *ranges = [NSMutableArray array];

        for (NSString *line in lines) {
            NSString *trim = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            if ([trim length] == 0) continue;
            NSArray *parts = [trim componentsSeparatedByString:@"-"];
            if ([parts count] != 2) continue;
            long long a = [parts[0] longLongValue];
            long long b = [parts[1] longLongValue];
            if (a > b) { long long t = a; a = b; b = t; }
            Range *r = [[Range alloc] init];
            r.min = a;
            r.max = b;
            [ranges addObject:r];
        }

        [ranges sortUsingComparator:^NSComparisonResult(Range *x, Range *y) {
            if (x.min != y.min) return (x.min < y.min) ? NSOrderedAscending : NSOrderedDescending;
            if (x.max != y.max) return (x.max < y.max) ? NSOrderedAscending : NSOrderedDescending;
            return NSOrderedSame;
        }];

        long long total = 0;
        long long curMin = 0, curMax = 0;
        BOOL first = YES;
        for (Range *r in ranges) {
            if (first) {
                curMin = r.min;
                curMax = r.max;
                first = NO;
            } else if (r.min <= curMax) {
                if (r.max > curMax) curMax = r.max;
            } else {
                total += (curMax - curMin + 1);
                curMin = r.min;
                curMax = r.max;
            }
        }
        if (!first) total += (curMax - curMin + 1);
        printf("Total fresh IDs: %lld\n", total);
    }
    return 0;
}
