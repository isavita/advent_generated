#import <Foundation/Foundation.h>

@interface RangeMap : NSObject
@property (nonatomic) NSInteger srcStart;
@property (nonatomic) NSInteger destStart;
@property (nonatomic) NSInteger length;
@end

@implementation RangeMap
@end

NSInteger convertNumber(NSInteger number, NSArray<RangeMap *> *ranges) {
    for (RangeMap *r in ranges) {
        if (number >= r.srcStart && number < r.srcStart + r.length) {
            return r.destStart + (number - r.srcStart);
        }
    }
    return number;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];

        NSMutableArray<NSNumber *> *seeds = [NSMutableArray array];
        NSMutableArray<NSArray<RangeMap *> *> *maps = [NSMutableArray array];
        NSMutableArray<RangeMap *> *currentRanges = [NSMutableArray array];

        for (NSString *line in lines) {
            if ([line containsString:@"map:"]) {
                if (currentRanges.count > 0) {
                    [maps addObject:[currentRanges copy]];
                    [currentRanges removeAllObjects];
                }
            } else if ([line hasPrefix:@"seeds:"]) {
                NSArray *seedStrs = [[line substringFromIndex:7] componentsSeparatedByString:@" "];
                for (NSString *s in seedStrs) {
                    [seeds addObject:@([s integerValue])];
                }
            } else {
                NSArray *numbers = [line componentsSeparatedByString:@" "];
                if (numbers.count == 3) {
                    RangeMap *range = [[RangeMap alloc] init];
                    range.destStart = [numbers[0] integerValue];
                    range.srcStart = [numbers[1] integerValue];
                    range.length = [numbers[2] integerValue];
                    [currentRanges addObject:range];
                }
            }
        }
        if (currentRanges.count > 0) {
            [maps addObject:[currentRanges copy]];
        }

        NSInteger minLocation = NSIntegerMax;
        for (NSNumber *seed in seeds) {
            NSInteger location = [seed integerValue];
            for (NSArray<RangeMap *> *m in maps) {
                location = convertNumber(location, m);
            }
            if (location < minLocation) {
                minLocation = location;
            }
        }

        NSLog(@"%ld", (long)minLocation);
    }
    return 0;
}