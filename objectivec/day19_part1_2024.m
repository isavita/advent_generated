
#import <Foundation/Foundation.h>

BOOL canMake(NSString *design, NSArray<NSString *> *patterns, NSArray<NSNumber *> *lengths) {
    NSUInteger n = design.length;
    if (n == 0) return YES;

    NSMutableArray<NSNumber *> *dp = [NSMutableArray arrayWithCapacity:n + 1];
    for (NSUInteger i = 0; i <= n; i++) [dp addObject:@(NO)];
    dp[0] = @(YES);

    for (NSUInteger i = 1; i <= n; i++) {
        for (NSUInteger p = 0; p < patterns.count; p++) {
            NSUInteger lp = lengths[p].unsignedIntegerValue;
            if (lp == 0 && i > 0) continue;
            if (i >= lp && dp[i - lp].boolValue) {
                NSRange r = NSMakeRange(i - lp, lp);
                if ([[design substringWithRange:r] isEqualToString:patterns[p]]) {
                    dp[i] = @(YES);
                    break;
                }
            }
        }
    }
    return dp[n].boolValue;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        if (!content) return 1;
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        if (lines.count == 0) return 1;

        NSString *patternsLine = [lines[0] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
        NSArray<NSString *> *rawPatterns = [patternsLine componentsSeparatedByString:@","];
        NSMutableArray<NSString *> *patterns = [NSMutableArray array];
        NSMutableArray<NSNumber *> *lengths = [NSMutableArray array];
        for (NSString *raw in rawPatterns) {
            NSString *p = [raw stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            [patterns addObject:p];
            [lengths addObject:@(p.length)];
        }

        NSUInteger start = 2; // skip possible separator line
        if (lines.count > 1 && [lines[1] length] == 0) start = 2;
        else start = 2; // as per original, second line ignored regardless

        NSUInteger count = 0;
        for (NSUInteger i = start; i < lines.count; i++) {
            NSString *design = [lines[i] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (design.length == 0) continue;
            if (canMake(design, patterns, lengths)) count++;
        }
        printf("%lu\n", (unsigned long)count);
    }
    return 0;
}
