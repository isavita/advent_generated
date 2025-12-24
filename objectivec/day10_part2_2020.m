
#import <Foundation/Foundation.h>

int compare(const void *a, const void *b) {
    return *(int *)a - *(int *)b;
}

long long countArrangements(int *a, int n) {
    long long *dp = calloc(n, sizeof(long long));
    dp[0] = 1;
    for (int i = 1; i < n; ++i) {
        for (int j = 1; j <= 3; ++j) {
            if (i - j >= 0 && a[i] - a[i - j] <= 3) dp[i] += dp[i - j];
        }
    }
    long long res = dp[n - 1];
    free(dp);
    return res;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
        NSMutableArray<NSNumber *> *vals = [NSMutableArray array];
        [vals addObject:@0];
        for (NSString *s in lines) {
            if (s.length) [vals addObject:@(s.intValue)];
        }
        int n = (int)vals.count;
        int *adapters = malloc((n + 2) * sizeof(int));
        for (int i = 0; i < n; ++i) adapters[i] = vals[i].intValue;
        qsort(adapters, n, sizeof(int), compare);
        adapters[n] = adapters[n - 1] + 3;
        n += 1;
        printf("%lld\n", countArrangements(adapters, n));
        free(adapters);
    }
    return 0;
}
