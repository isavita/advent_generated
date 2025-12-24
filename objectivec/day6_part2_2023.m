
#import <Foundation/Foundation.h>

long long waysToWin(long long T, long long R) {
    long long lo = 0, hi = T, mid, first = T, last = -1;
    while (lo <= hi) {
        mid = (lo + hi) >> 1;
        if (mid * (T - mid) > R) { first = mid; hi = mid - 1; }
        else { lo = mid + 1; }
    }
    lo = 0; hi = T;
    while (lo <= hi) {
        mid = (lo + hi) >> 1;
        if (mid * (T - mid) > R) { last = mid; lo = mid + 1; }
        else { hi = mid - 1; }
    }
    return last - first + 1;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray *lines = [txt componentsSeparatedByCharactersInSet:
                         [NSCharacterSet newlineCharacterSet]];
        long long T = 0, D = 0;
        for (NSString *l in lines) {
            NSRange c = [l rangeOfString:@":"];
            if (c.location == NSNotFound) continue;
            NSString *digits = [[l substringFromIndex:c.location + 1]
                              stringByTrimmingCharactersInSet:
                              [NSCharacterSet whitespaceCharacterSet]];
            digits = [[digits componentsSeparatedByCharactersInSet:
                       [NSCharacterSet whitespaceCharacterSet]]
                      componentsJoinedByString:@""];
            if (T == 0) T = [digits longLongValue];
            else D = [digits longLongValue];
        }
        printf("%lld\n", waysToWin(T, D));
    }
    return 0;
}
