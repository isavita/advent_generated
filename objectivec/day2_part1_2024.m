#import <Foundation/Foundation.h>

static BOOL isSafe(NSArray<NSNumber *> *a) {
    if (a.count < 2) return NO;
    long d0 = a[1].longValue - a[0].longValue;
    if (d0 == 0 || llabs(d0) > 3) return NO;
    BOOL inc = d0 > 0;
    for (NSUInteger i = 1; i < a.count - 1; ++i) {
        long d = a[i + 1].longValue - a[i].longValue;
        if (d == 0 || llabs(d) > 3) return NO;
        if ((inc && d < 0) || (!inc && d > 0)) return NO;
    }
    return YES;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = [[NSFileManager defaultManager] currentDirectoryPath];
        NSString *file = [path stringByAppendingPathComponent:@"input.txt"];
        NSString *text = [NSString stringWithContentsOfFile:file
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        if (!text) return 1;
        NSInteger safe = 0;
        NSArray<NSString *> *lines = [text componentsSeparatedByCharactersInSet:
                                    [NSCharacterSet newlineCharacterSet]];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray<NSString *> *parts = [line componentsSeparatedByString:@" "];
            NSMutableArray<NSNumber *> *nums = [NSMutableArray arrayWithCapacity:parts.count];
            for (NSString *p in parts) {
                long v = p.longLongValue;
                [nums addObject:@(v)];
            }
            if (isSafe(nums)) ++safe;
        }
        printf("%ld\n", (long)safe);
    }
    return 0;
}