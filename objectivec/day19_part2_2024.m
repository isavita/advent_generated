
#import <Foundation/Foundation.h>

static long long countWays(NSString *design, NSArray<NSString*> *patterns) {
    NSUInteger n = design.length;
    NSMutableArray *dp = [NSMutableArray arrayWithCapacity:n+1];
    for (NSUInteger i=0;i<=n;i++) dp[i] = @(0);
    dp[0] = @(1);
    for (NSUInteger i=1;i<=n;i++) {
        for (NSString *pat in patterns) {
            NSUInteger lp = pat.length;
            if (lp<=i) {
                NSRange rng = NSMakeRange(i-lp, lp);
                if ([design compare:pat options:0 range:rng] == NSOrderedSame) {
                    dp[i] = @([dp[i] longLongValue] + [dp[i-lp] longLongValue]);
                }
            }
        }
    }
    return [dp[n] longLongValue];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                 encoding:NSUTF8StringEncoding
                                                    error:nil];
        if (!txt) return 1;
        NSArray *lines = [txt componentsSeparatedByCharactersInSet:
                         [NSCharacterSet newlineCharacterSet]];
        if (lines.count<2) return 1;
        NSArray *parts = [[lines[0] stringByTrimmingCharactersInSet:
                        [NSCharacterSet whitespaceCharacterSet]]
                       componentsSeparatedByString:@","];
        NSMutableArray *patterns = [NSMutableArray array];
        for (NSString *p in parts) {
            NSString *t = [p stringByTrimmingCharactersInSet:
                          [NSCharacterSet whitespaceCharacterSet]];
            if (t.length) [patterns addObject:t];
        }
        long long total = 0;
        for (NSUInteger i=2;i<lines.count;i++) {
            NSString *d = [lines[i] stringByTrimmingCharactersInSet:
                          [NSCharacterSet whitespaceCharacterSet]];
            if (d.length) total += countWays(d, patterns);
        }
        printf("%lld\n", total);
    }
    return 0;
}
