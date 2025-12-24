
#import <Foundation/Foundation.h>

static BOOL isCorrect(NSArray<NSNumber *> *update, NSArray<NSArray<NSNumber *> *> *rules) {
    NSMutableDictionary<NSNumber *, NSNumber *> *pos = [NSMutableDictionary dictionaryWithCapacity:update.count];
    for (NSUInteger i = 0; i < update.count; i++) {
        pos[update[i]] = @(i);
    }
    for (NSArray<NSNumber *> *rule in rules) {
        NSNumber *p1 = pos[rule[0]];
        NSNumber *p2 = pos[rule[1]];
        if (p1 && p2 && [p1 compare:p2] == NSOrderedDescending) return NO;
    }
    return YES;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        if (!data) return 1;

        NSMutableArray<NSArray<NSNumber *> *> *rules = [NSMutableArray array];
        NSMutableArray<NSArray<NSNumber *> *> *updates = [NSMutableArray array];
        BOOL parsingRules = YES;

        for (NSString *line in [data componentsSeparatedByCharactersInSet:
                               [NSCharacterSet newlineCharacterSet]]) {
            line = [line stringByTrimmingCharactersInSet:
                    [NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (line.length == 0) continue;

            NSRange pipe = [line rangeOfString:@"|"];
            if (parsingRules && pipe.location != NSNotFound) {
                int x = [[line substringToIndex:pipe.location] intValue];
                int y = [[line substringFromIndex:pipe.location + 1] intValue];
                [rules addObject:@[@(x), @(y)]];
                continue;
            }
            parsingRules = NO;

            NSArray<NSString *> *parts = [line componentsSeparatedByString:@","];
            NSMutableArray<NSNumber *> *row = [NSMutableArray arrayWithCapacity:parts.count];
            for (NSString *p in parts) [row addObject:@([p intValue])];
            [updates addObject:row];
        }

        long long total = 0;
        for (NSArray<NSNumber *> *u in updates) {
            if (isCorrect(u, rules)) total += [u[u.count / 2] longLongValue];
        }
        printf("%lld\n", total);
    }
    return 0;
}
