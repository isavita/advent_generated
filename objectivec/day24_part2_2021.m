
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray *lines = [data componentsSeparatedByString:@"\n"];

        NSMutableArray *k = [NSMutableArray array];
        NSMutableArray *l = [NSMutableArray array];
        NSMutableArray *m = [NSMutableArray array];

        for (NSUInteger i = 0; i < lines.count; i++) {
            NSString *line = lines[i];
            switch (i % 18) {
                case 4: {
                    NSInteger v = 0;
                    sscanf([line UTF8String], "div z %ld", &v);
                    [l addObject:@(v)];
                    break;
                }
                case 5: {
                    NSInteger v = 0;
                    sscanf([line UTF8String], "add x %ld", &v);
                    [k addObject:@(v)];
                    break;
                }
                case 15: {
                    NSInteger v = 0;
                    sscanf([line UTF8String], "add y %ld", &v);
                    [m addObject:@(v)];
                    break;
                }
            }
        }

        NSMutableDictionary *constraints = [NSMutableDictionary dictionary];
        NSMutableArray *stack = [NSMutableArray array];

        for (NSUInteger i = 0; i < l.count; i++) {
            if ([l[i] intValue] == 1) {
                [stack addObject:@(i)];
            } else {
                NSInteger pop = [stack.lastObject intValue];
                [stack removeLastObject];
                constraints[@(pop)] = @[@(i), @([m[pop] intValue] + [k[i] intValue])];
            }
        }

        NSMutableArray *min = [NSMutableArray arrayWithCapacity:14];
        for (int i = 0; i < 14; i++) [min addObject:@1];

        for (NSNumber *i in constraints) {
            NSArray *pair = constraints[i];
            NSInteger j = [pair[0] intValue];
            NSInteger delta = [pair[1] intValue];
            NSInteger vmin = 1;
            while (vmin + delta < 1) vmin++;
            min[i.intValue] = @(vmin);
            min[j] = @(vmin + delta);
        }

        long long result = 0;
        for (NSNumber *n in min) {
            result = result * 10 + [n intValue];
        }
        printf("%lld\n", result);
    }
    return 0;
}
