#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"File reading error: %@", error);
            return 1;
        }

        NSArray *lines = [data componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSInteger checksum = 0;

        for (NSString *line in lines) {
            NSArray *nums = [line componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            NSInteger minVal = NSIntegerMax, maxVal = NSIntegerMin;

            for (NSString *numStr in nums) {
                NSInteger num = [numStr integerValue];
                if (num < minVal) minVal = num;
                if (num > maxVal) maxVal = num;
            }

            checksum += (maxVal - minVal);
        }

        NSLog(@"%ld", (long)checksum);
    }
    return 0;
}