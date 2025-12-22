
#import <Foundation/Foundation.h>

static long long concatNums(long long a, long long b) {
    NSString *s = [NSString stringWithFormat:@"%lld%lld", a, b];
    return [s longLongValue];
}

static BOOL evaluate(long long target, NSArray<NSNumber *> *nums) {
    NSUInteger n = nums.count;
    if (n == 0) return NO;
    if (n == 1) return [nums[0] longLongValue] == target;

    NSUInteger opsCount = n - 1;
    NSMutableArray<NSNumber *> *ops = [NSMutableArray arrayWithCapacity:opsCount];
    for (NSUInteger i = 0; i < opsCount; ++i) ops[i] = @0;

    while (YES) {
        long long cur = [nums[0] longLongValue];
        BOOL ok = YES;
        for (NSUInteger i = 0; i < opsCount; ++i) {
            long long nxt = [nums[i + 1] longLongValue];
            switch ([ops[i] intValue]) {
                case 0: // +
                    cur += nxt;
                    break;
                case 1: // *
                    cur *= nxt;
                    break;
                case 2: // ||
                    cur = concatNums(cur, nxt);
                    break;
            }
        }
        if (ok && cur == target) return YES;

        NSInteger k = opsCount - 1;
        while (k >= 0) {
            ops[k] = @([ops[k] intValue] + 1);
            if ([ops[k] intValue] < 3) break;
            ops[k] = @0;
            k--;
        }
        if (k < 0) break;
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;

        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        long long total = 0;

        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSRange colon = [line rangeOfString:@":"];
            if (colon.location == NSNotFound) continue;

            NSString *targetStr = [line substringToIndex:colon.location];
            NSString *numsStr = [[line substringFromIndex:colon.location + 1] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];

            long long target = [targetStr longLongValue];
            NSArray<NSString *> *numTokens = [numsStr componentsSeparatedByString:@" "];
            NSMutableArray<NSNumber *> *nums = [NSMutableArray array];
            for (NSString *tok in numTokens) {
                if (tok.length == 0) continue;
                [nums addObject:@([tok longLongValue])];
            }

            if (evaluate(target, nums)) total += target;
        }

        printf("%lld\n", total);
    }
    return 0;
}
