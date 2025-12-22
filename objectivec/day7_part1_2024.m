
#import <Foundation/Foundation.h>

BOOL evaluateExpression(long long target, int *nums, int count) {
    if (count == 1) return (long long)nums[0] == target;
    int ops = count - 1;
    long long limit = 1LL << ops;
    for (long long mask = 0; mask < limit; ++mask) {
        long long res = nums[0];
        long long m = mask;
        for (int i = 0; i < ops; ++i) {
            if ((m & 1) == 0)
                res += nums[i + 1];
            else
                res *= nums[i + 1];
            m >>= 1;
        }
        if (res == target) return YES;
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
        int nums[64];
        for (NSString *rawLine in lines) {
            NSString *line = [rawLine stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (line.length == 0) continue;
            NSRange colon = [line rangeOfString:@":"];
            if (colon.location == NSNotFound) continue;
            long long target = [[line substringToIndex:colon.location] longLongValue];
            NSString *numPart = [[line substringFromIndex:colon.location + 1] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            NSArray<NSString *> *tokens = [numPart componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            int count = 0;
            for (NSString *t in tokens) {
                if (t.length == 0) continue;
                nums[count++] = [t intValue];
                if (count == 64) break;
            }
            if (count && evaluateExpression(target, nums, count))
                total += target;
        }
        printf("%lld\n", total);
    }
    return 0;
}
