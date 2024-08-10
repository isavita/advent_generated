#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"File reading error: %@", error);
            return 1;
        }

        NSArray *lines = [data componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSInteger sum = 0;

        for (NSString *line in lines) {
            NSArray *numsStr = [line componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            NSMutableArray *nums = [NSMutableArray array];

            for (NSString *numStr in numsStr) {
                NSInteger num = [numStr integerValue];
                if (num != 0 || [numStr isEqualToString:@"0"]) {
                    [nums addObject:@(num)];
                }
            }

            for (NSUInteger i = 0; i < nums.count; i++) {
                for (NSUInteger j = 0; j < nums.count; j++) {
                    if (i != j) {
                        NSInteger num1 = [nums[i] integerValue];
                        NSInteger num2 = [nums[j] integerValue];
                        if (num1 % num2 == 0) {
                            sum += num1 / num2;
                        }
                    }
                }
            }
        }

        NSLog(@"%ld", (long)sum);
    }
    return 0;
}