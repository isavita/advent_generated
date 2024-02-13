#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *boxIDs = [input componentsSeparatedByString:@"\n"];
        
        int twoCount = 0;
        int threeCount = 0;
        
        for (NSString *boxID in boxIDs) {
            NSMutableDictionary *letterCounts = [NSMutableDictionary dictionary];
            for (int i = 0; i < boxID.length; i++) {
                NSString *letter = [boxID substringWithRange:NSMakeRange(i, 1)];
                if (letterCounts[letter]) {
                    letterCounts[letter] = @([letterCounts[letter] intValue] + 1);
                } else {
                    letterCounts[letter] = @1;
                }
            }
            
            BOOL hasTwo = NO;
            BOOL hasThree = NO;
            for (NSNumber *count in [letterCounts allValues]) {
                if ([count intValue] == 2) {
                    hasTwo = YES;
                } else if ([count intValue] == 3) {
                    hasThree = YES;
                }
            }
            
            if (hasTwo) {
                twoCount++;
            }
            if (hasThree) {
                threeCount++;
            }
        }
        
        printf("Checksum: %d\n", twoCount * threeCount);
    }
    return 0;
}