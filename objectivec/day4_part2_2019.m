#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *range = [input componentsSeparatedByString:@"-"];
        int lowerBound = [range[0] intValue];
        int upperBound = [range[1] intValue];
        
        int countPart1 = 0;
        int countPart2 = 0;
        
        for (int i = lowerBound; i <= upperBound; i++) {
            NSString *password = [NSString stringWithFormat:@"%d", i];
            
            BOOL hasAdjacentDigits = NO;
            BOOL hasAdjacentPair = NO;
            BOOL neverDecrease = YES;
            
            for (int j = 0; j < password.length - 1; j++) {
                if ([password characterAtIndex:j] == [password characterAtIndex:j+1]) {
                    hasAdjacentDigits = YES;
                    if (j == 0 || [password characterAtIndex:j] != [password characterAtIndex:j-1]) {
                        if (j == password.length - 2 || [password characterAtIndex:j] != [password characterAtIndex:j+2]) {
                            hasAdjacentPair = YES;
                        }
                    }
                }
                if ([password characterAtIndex:j] > [password characterAtIndex:j+1]) {
                    neverDecrease = NO;
                    break;
                }
            }
            
            if (hasAdjacentDigits && neverDecrease) {
                countPart1++;
                if (hasAdjacentPair) {
                    countPart2++;
                }
            }
        }
        
        printf("Part 1: %d\n", countPart1);
        printf("Part 2: %d\n", countPart2);
    }
    return 0;
}