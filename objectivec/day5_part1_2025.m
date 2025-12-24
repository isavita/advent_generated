
#import <Foundation/Foundation.h>

@interface Range : NSObject
@property long long min, max;
- (instancetype)initWithMin:(long long)min max:(long long)max;
@end
@implementation Range
- (instancetype)initWithMin:(long long)min max:(long long)max {
    self = [super init];
    if (self) { _min = min; _max = max; }
    return self;
}
@end

static BOOL contains(NSArray<Range *> *arr, long long x) {
    NSUInteger l = 0, r = arr.count;
    while (l < r) {
        NSUInteger m = (l + r) >> 1;
        Range *R = arr[m];
        if (x < R.min) r = m;
        else if (x > R.max) l = m + 1;
        else return YES;
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:&err];
    if (!txt) return 1;
    NSArray<NSString *> *lines = [txt componentsSeparatedByCharactersInSet:
                                  [NSCharacterSet newlineCharacterSet]];
    NSMutableArray<Range *> *ranges = [NSMutableArray array];
    BOOL parsingRanges = YES;
    long long fresh = 0;
    for (NSString *raw in lines) {
        NSString *s = [raw stringByTrimmingCharactersInSet:
                       [NSCharacterSet whitespaceCharacterSet]];
        if (s.length == 0) {
            if (parsingRanges) {
                parsingRanges = NO;
                [ranges sortUsingComparator:^NSComparisonResult(Range *a, Range *b) {
                    if (a.min != b.min) return a.min < b.min ? NSOrderedAscending : NSOrderedDescending;
                    if (a.max != b.max) return a.max < b.max ? NSOrderedAscending : NSOrderedDescending;
                    return NSOrderedSame;
                }];
                NSMutableArray<Range *> *m = [NSMutableArray array];
                for (Range *R in ranges) {
                    if (m.count == 0 || R.min > m.lastObject.max) {
                        [m addObject:R];
                    } else if (R.max > m.lastObject.max) {
                        m.lastObject.max = R.max;
                    }
                }
                ranges = m;
            }
            continue;
        }
        if (parsingRanges) {
            NSArray<NSString *> *parts = [s componentsSeparatedByString:@"-"];
            if (parts.count != 2) return 1;
            long long mn = [parts[0] longLongValue];
            long long mx = [parts[1] longLongValue];
            [ranges addObject:[[Range alloc] initWithMin:mn max:mx]];
        } else {
            long long id = [s longLongValue];
            if (contains(ranges, id)) fresh++;
        }
    }
    printf("Number of fresh ingredients: %lld\n", fresh);
    return 0;
    }
}
