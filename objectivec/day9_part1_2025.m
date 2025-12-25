#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path
                                                     encoding:NSUTF8StringEncoding
                                                        error:nil];
        if (!content) return 0;

        NSMutableArray<NSNumber *> *xs = [NSMutableArray array];
        NSMutableArray<NSNumber *> *ys = [NSMutableArray array];

        [content enumerateLinesUsingBlock:^(NSString *line, BOOL *stop) {
            NSArray *parts = [line componentsSeparatedByString:@","];
            if (parts.count == 2) {
                int x = [parts[0] intValue];
                int y = [parts[1] intValue];
                [xs addObject:@(x)];
                [ys addObject:@(y)];
            }
        }];

        NSUInteger n = xs.count;
        if (n == 0) return 0;

        long long maxArea = 1;
        for (NSUInteger i = 0; i < n; ++i) {
            int x1 = [xs[i] intValue];
            int y1 = [ys[i] intValue];
            for (NSUInteger j = i + 1; j < n; ++j) {
                long long w = llabs(x1 - [xs[j] intValue]) + 1;
                long long h = llabs(y1 - [ys[j] intValue]) + 1;
                long long area = w * h;
                if (area > maxArea) maxArea = area;
            }
        }
        printf("%lld\n", maxArea);
    }
    return 0;
}