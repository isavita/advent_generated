#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *frequencyChanges = [input componentsSeparatedByString:@"\n"];
        
        // Part One
        int resultingFrequency = 0;
        for (NSString *change in frequencyChanges) {
            resultingFrequency += [change intValue];
        }
        printf("Part One - Resulting Frequency: %d\n", resultingFrequency);
        
        // Part Two
        NSMutableSet *seenFrequencies = [NSMutableSet set];
        int currentFrequency = 0;
        BOOL foundDuplicate = NO;
        
        while (!foundDuplicate) {
            for (NSString *change in frequencyChanges) {
                currentFrequency += [change intValue];
                if ([seenFrequencies containsObject:@(currentFrequency)]) {
                    printf("Part Two - First Duplicate Frequency: %d\n", currentFrequency);
                    foundDuplicate = YES;
                    break;
                }
                [seenFrequencies addObject:@(currentFrequency)];
            }
        }
    }
    return 0;
}