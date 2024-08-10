#import <Foundation/Foundation.h>

NSArray* applyFFT(NSArray *input) {
    NSArray *basePattern = @[@0, @1, @0, @-1];
    NSMutableArray *output = [NSMutableArray arrayWithCapacity:input.count];
    
    for (NSInteger i = 0; i < input.count; i++) {
        NSInteger sum = 0;
        for (NSInteger j = 0; j < input.count; j++) {
            NSInteger patternValue = [basePattern[(j + 1) / (i + 1) % basePattern.count] integerValue];
            sum += [input[j] integerValue] * patternValue;
        }
        [output addObject:@(labs(sum) % 10)];
    }
    return output;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSMutableArray *digits = [NSMutableArray arrayWithCapacity:input.length];
        
        for (NSUInteger i = 0; i < input.length; i++) {
            [digits addObject:@([[input substringWithRange:NSMakeRange(i, 1)] integerValue])];
        }

        for (NSInteger phase = 0; phase < 100; phase++) {
            digits = [applyFFT(digits) mutableCopy];
        }

        for (NSUInteger i = 0; i < 8; i++) {
            printf("%ld", [digits[i] integerValue]);
        }
        printf("\n");
    }
    return 0;
}