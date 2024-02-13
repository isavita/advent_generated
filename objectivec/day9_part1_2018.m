#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSString *inputPath = @"input.txt";
        NSString *inputString = [NSString stringWithContentsOfFile:inputPath encoding:NSUTF8StringEncoding error:nil];
        NSArray *inputArr = [inputString componentsSeparatedByString:@" "];
        int players = [inputArr[0] intValue];
        int lastMarble = [inputArr[6] intValue];
        
        NSMutableArray *circle = [NSMutableArray arrayWithObject:@0];
        NSMutableDictionary *scores = [NSMutableDictionary dictionary];
        int currentPlayer = 0;
        int currentMarbleIndex = 0;
        
        for (int i = 1; i <= lastMarble; i++) {
            if (i % 23 == 0) {
                int score = i;
                currentMarbleIndex = (currentMarbleIndex - 7 + circle.count) % circle.count;
                score += [circle[currentMarbleIndex] intValue];
                [circle removeObjectAtIndex:currentMarbleIndex];
                scores[@(currentPlayer)] = @([scores[@(currentPlayer)] intValue] + score);
            } else {
                currentMarbleIndex = (currentMarbleIndex + 2) % circle.count;
                if (currentMarbleIndex == 0) {
                    [circle addObject:@(i)];
                    currentMarbleIndex = circle.count - 1;
                } else {
                    [circle insertObject:@(i) atIndex:currentMarbleIndex];
                }
            }
            currentPlayer = (currentPlayer + 1) % players;
        }
        
        int maxScore = 0;
        for (NSNumber *score in scores.allValues) {
            if ([score intValue] > maxScore) {
                maxScore = [score intValue];
            }
        }
        
        printf("%d\n", maxScore);
    }
    return 0;
}