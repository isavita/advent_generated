#import <Foundation/Foundation.h>

int64_t applyMask(int64_t value, NSString *mask) {
    int64_t result = 0;
    for (int i = 0; i < 36; i++) {
        int64_t bitValue = 1LL << (35 - i);
        if ([mask characterAtIndex:i] == '1') {
            result |= bitValue;
        } else if ([mask characterAtIndex:i] == 'X') {
            result |= (value & bitValue);
        }
    }
    return result;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) return 1;

        NSMutableDictionary<NSNumber *, NSNumber *> *mem = [NSMutableDictionary dictionary];
        NSString *mask = @"";
        NSRegularExpression *reMem = [NSRegularExpression regularExpressionWithPattern:@"mem\\[(\\d+)\\] = (\\d+)" options:0 error:nil];

        for (NSString *line in [input componentsSeparatedByString:@"\n"]) {
            if ([line hasPrefix:@"mask = "]) {
                mask = [line substringFromIndex:7];
            } else {
                NSTextCheckingResult *match = [reMem firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
                if (match) {
                    int64_t address = [[line substringWithRange:[match rangeAtIndex:1]] longLongValue];
                    int64_t value = [[line substringWithRange:[match rangeAtIndex:2]] longLongValue];
                    mem[@(address)] = @(applyMask(value, mask));
                }
            }
        }

        int64_t sum = 0;
        for (NSNumber *value in mem.allValues) {
            sum += value.longLongValue;
        }

        printf("%lld\n", sum);
    }
    return 0;
}