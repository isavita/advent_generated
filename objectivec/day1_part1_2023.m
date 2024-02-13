#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }

        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        int sum = 0;

        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }

            int firstDigit = -1;
            int lastDigit = -1;

            for (int i = 0; i < [line length]; i++) {
                unichar character = [line characterAtIndex:i];
                if ([[NSCharacterSet decimalDigitCharacterSet] characterIsMember:character]) {
                    if (firstDigit == -1) {
                        firstDigit = character - '0';
                    }
                    lastDigit = character - '0';
                }
            }

            if (firstDigit != -1 && lastDigit != -1) {
                int value = [[NSString stringWithFormat:@"%d%d", firstDigit, lastDigit] intValue];
                sum += value;
            }
        }

        printf("%d\n", sum);
    }
    return 0;
}