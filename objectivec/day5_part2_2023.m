#import <Foundation/Foundation.h>

@interface RangeMap : NSObject
@property (nonatomic) NSInteger srcStart;
@property (nonatomic) NSInteger destStart;
@property (nonatomic) NSInteger length;
- (instancetype)initWithSrcStart:(NSInteger)srcStart destStart:(NSInteger)destStart length:(NSInteger)length;
@end

@implementation RangeMap
- (instancetype)initWithSrcStart:(NSInteger)srcStart destStart:(NSInteger)destStart length:(NSInteger)length {
    self = [super init];
    if (self) {
        _srcStart = srcStart;
        _destStart = destStart;
        _length = length;
    }
    return self;
}
@end

NSInteger reverseConvertNumber(NSInteger number, NSArray<RangeMap *> *ranges) {
    for (NSInteger i = ranges.count - 1; i >= 0; i--) {
        RangeMap *r = ranges[i];
        if (number >= r.destStart && number < r.destStart + r.length) {
            return r.srcStart + (number - r.destStart);
        }
    }
    return number;
}

BOOL isInSeedRanges(NSInteger number, NSArray<NSArray<NSNumber *> *> *ranges) {
    for (NSArray<NSNumber *> *r in ranges) {
        if (number >= r[0].integerValue && number < r[0].integerValue + r[1].integerValue) {
            return YES;
        }
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            return 1;
        }

        NSArray<NSString *> *lines = [input componentsSeparatedByString:@"\n"];
        NSMutableArray<NSArray<NSNumber *> *> *seedRanges = [NSMutableArray array];
        NSMutableArray<RangeMap *> *currentRanges = [NSMutableArray array];
        NSMutableArray<NSArray<RangeMap *> *> *maps = [NSMutableArray array];

        for (NSString *line in lines) {
            if ([line containsString:@"map:"]) {
                if (currentRanges.count > 0) {
                    [maps addObject:[currentRanges copy]];
                    [currentRanges removeAllObjects];
                }
            } else if ([line hasPrefix:@"seeds:"]) {
                NSArray<NSString *> *seedStrs = [[line substringFromIndex:7] componentsSeparatedByString:@" "];
                for (NSInteger i = 0; i < seedStrs.count; i += 2) {
                    NSInteger start = seedStrs[i].integerValue;
                    NSInteger length = seedStrs[i + 1].integerValue;
                    [seedRanges addObject:@[@(start), @(length)]];
                }
            } else {
                NSArray<NSString *> *numbers = [line componentsSeparatedByString:@" "];
                if (numbers.count == 3) {
                    NSInteger destStart = numbers[0].integerValue;
                    NSInteger srcStart = numbers[1].integerValue;
                    NSInteger length = numbers[2].integerValue;
                    [currentRanges addObject:[[RangeMap alloc] initWithSrcStart:srcStart destStart:destStart length:length]];
                }
            }
        }
        if (currentRanges.count > 0) {
            [maps addObject:[currentRanges copy]];
        }

        for (NSInteger location = 0; ; location++) {
            NSInteger seed = location;
            for (NSInteger i = maps.count - 1; i >= 0; i--) {
                seed = reverseConvertNumber(seed, maps[i]);
            }
            if (isInSeedRanges(seed, seedRanges)) {
                NSLog(@"%ld", (long)location);
                break;
            }
        }
    }
    return 0;
}