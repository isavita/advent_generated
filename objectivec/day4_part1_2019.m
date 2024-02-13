#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *range = [input componentsSeparatedByString:@"-"];
        int lowerBound = [range[0] intValue];
        int upperBound = [range[1] intValue];
        
        int count = 0;
        for (int i = lowerBound; i <= upperBound; i++) {
            NSString *password = [NSString stringWithFormat:@"%d", i];
            BOOL hasAdjacentDigits = NO;
            BOOL neverDecrease = YES;
            
            for (int j = 0; j < password.length - 1; j++) {
                if ([password characterAtIndex:j] == [password characterAtIndex:j + 1]) {
                    hasAdjacentDigits = YES;
                }
                if ([password characterAtIndex:j] > [password characterAtIndex:j + 1]) {
                    neverDecrease = NO;
                    break;
                }
            }
            
            if (hasAdjacentDigits && neverDecrease) {
                count++;
            }
        }
        
        printf("%d\n", count);
    }
    return 0;
}