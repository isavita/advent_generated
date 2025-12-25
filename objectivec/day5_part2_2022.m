
#import <Foundation/Foundation.h>

NSArray<NSMutableArray<NSNumber *> *> *reverseStacks(NSArray<NSMutableArray<NSNumber *> *> *src) {
    NSMutableArray *dst = [NSMutableArray arrayWithCapacity:src.count];
    for (NSMutableArray *s in src) {
        NSMutableArray *r = [NSMutableArray arrayWithCapacity:s.count];
        for (NSInteger i = s.count - 1; i >= 0; --i) {
            [r addObject:s[i]];
        }
        [dst addObject:r];
    }
    return dst;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        NSMutableArray<NSString *> *input = [NSMutableArray array];
        NSMutableArray<NSString *> *steps = [NSMutableArray array];
        BOOL readingInput = YES;
        for (NSString *line in lines) {
            if (line.length == 0) {
                readingInput = NO;
                continue;
            }
            if (readingInput) {
                [input addObject:line];
            } else {
                [steps addObject:line];
            }
        }

        NSUInteger stackCount = (input[0].length + 1) / 4;
        NSMutableArray<NSMutableArray<NSNumber *> *> *stacks = [NSMutableArray arrayWithCapacity:stackCount];
        for (NSUInteger i = 0; i < stackCount; ++i) {
            [stacks addObject:[NSMutableArray array]];
        }

        for (NSString *line in input) {
            for (NSUInteger i = 0; i < line.length; ++i) {
                unichar c = [line characterAtIndex:i];
                if (c >= 'A' && c <= 'Z') {
                    NSUInteger idx = (i - 1) / 4;
                    [stacks[idx] addObject:@(c)];
                }
            }
        }

        NSArray *procStacks = reverseStacks(stacks);
        NSMutableArray<NSMutableArray<NSNumber *> *> *working = [procStacks mutableCopy];

        for (NSString *step in steps) {
            NSArray *tok = [step componentsSeparatedByString:@" "];
            NSInteger n = [tok[1] integerValue];
            NSInteger from = [tok[3] integerValue] - 1;
            NSInteger to = [tok[5] integerValue] - 1;

            NSMutableArray<NSNumber *> *src = working[from];
            NSMutableArray<NSNumber *> *dst = working[to];
            NSRange range = NSMakeRange(src.count - n, n);
            NSArray *slice = [src subarrayWithRange:range];
            [dst addObjectsFromArray:slice];
            [src removeObjectsInRange:range];
        }

        NSMutableString *result = [NSMutableString string];
        for (NSMutableArray<NSNumber *> *s in working) {
            [result appendFormat:@"%C", (unichar)[s.lastObject unsignedShortValue]];
        }
        printf("%s\n", result.UTF8String);
    }
    return 0;
}
