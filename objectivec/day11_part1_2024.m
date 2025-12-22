
#import <Foundation/Foundation.h>

static NSString *trimZero(NSString *s) {
    NSRange r = [s rangeOfString:@"^0*(?=.)" options:NSRegularExpressionSearch];
    if (r.location != NSNotFound) s = [s substringFromIndex:r.location + r.length];
    return s.length ? s : @"0";
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSMutableArray<NSString *> *stones =
            [[NSMutableArray alloc] initWithArray:[txt componentsSeparatedByCharactersInSet:
                [NSCharacterSet whitespaceAndNewlineCharacterSet]]];
        [stones removeObject:@""];

        for (int i = 0; i < 25; i++) {
            NSMutableArray *next = [NSMutableArray array];
            for (NSString *s in stones) {
                if ([s isEqualToString:@"0"]) {
                    [next addObject:@"1"];
                } else if (s.length % 2 == 0) {
                    NSString *l = trimZero([s substringToIndex:s.length/2]);
                    NSString *r = trimZero([s substringFromIndex:s.length/2]);
                    [next addObject:l];
                    [next addObject:r];
                } else {
                    long n = [s longLongValue];
                    [next addObject:[NSString stringWithFormat:@"%ld", n * 2024]];
                }
            }
            stones = next;
        }
        printf("%lu\n", (unsigned long)stones.count);
    }
    return 0;
}
