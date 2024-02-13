#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        
        NSInteger totalScore = 0;
        NSInteger garbageCount = 0;
        NSInteger groupScore = 0;
        BOOL inGarbage = NO;
        BOOL ignoreNext = NO;
        
        for (int i = 0; i < input.length; i++) {
            unichar c = [input characterAtIndex:i];
            
            if (ignoreNext) {
                ignoreNext = NO;
                continue;
            }
            
            if (inGarbage) {
                if (c == '!') {
                    ignoreNext = YES;
                } else if (c == '>') {
                    inGarbage = NO;
                } else {
                    garbageCount++;
                }
            } else {
                if (c == '{') {
                    groupScore++;
                    totalScore += groupScore;
                } else if (c == '}') {
                    groupScore--;
                } else if (c == '<') {
                    inGarbage = YES;
                }
            }
        }
        
        printf("Total Score: %ld\n", totalScore);
        printf("Garbage Count: %ld\n", garbageCount);
    }
    return 0;
}