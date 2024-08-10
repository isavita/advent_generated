#import <Foundation/Foundation.h>

NSArray *generateAddresses(NSString *mask, long long address) {
    NSMutableArray *floating = [NSMutableArray array];
    NSMutableArray *addresses = [NSMutableArray array];

    for (NSInteger i = 0; i < mask.length; i++) {
        unichar bit = [mask characterAtIndex:i];
        if (bit == '1') {
            address |= (1LL << (35 - i));
        } else if (bit == 'X') {
            [floating addObject:@(35 - i)];
        }
    }

    NSInteger count = 1 << floating.count;
    for (NSInteger i = 0; i < count; i++) {
        long long modAddress = address;
        for (NSInteger j = 0; j < floating.count; j++) {
            NSInteger pos = [floating[j] integerValue];
            if (i & (1 << j)) {
                modAddress |= (1LL << pos);
            } else {
                modAddress &= ~(1LL << pos);
            }
        }
        [addresses addObject:@(modAddress)];
    }
    return addresses;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) return 1;

        NSMutableDictionary *mem = [NSMutableDictionary dictionary];
        NSString *mask = @"";
        NSRegularExpression *reMem = [NSRegularExpression regularExpressionWithPattern:@"mem\\[(\\d+)\\] = (\\d+)" options:0 error:nil];

        for (NSString *line in [input componentsSeparatedByString:@"\n"]) {
            if ([line hasPrefix:@"mask = "]) {
                mask = [line substringFromIndex:7];
            } else {
                NSTextCheckingResult *match = [reMem firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
                if (match) {
                    long long address = [[line substringWithRange:[match rangeAtIndex:1]] longLongValue];
                    long long value = [[line substringWithRange:[match rangeAtIndex:2]] longLongValue];
                    NSArray *addresses = generateAddresses(mask, address);
                    for (NSNumber *addr in addresses) {
                        mem[addr] = @(value);
                    }
                }
            }
        }

        long long sum = 0;
        for (NSNumber *value in mem.allValues) {
            sum += [value longLongValue];
        }
        NSLog(@"%lld", sum);
    }
    return 0;
}