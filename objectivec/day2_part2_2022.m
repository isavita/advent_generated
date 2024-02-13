#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        int totalScore = 0;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            
            unichar opponent = [line characterAtIndex:0];
            unichar roundEnd = [line characterAtIndex:2];
            
            unichar yourMove = ' ';
            if (roundEnd == 'X') {
                if (opponent == 'A') {
                    yourMove = 'Z';
                } else if (opponent == 'B') {
                    yourMove = 'X';
                } else {
                    yourMove = 'Y';
                }
            } else if (roundEnd == 'Y') {
                if (opponent == 'A') {
                    yourMove = 'X';
                } else if (opponent == 'B') {
                    yourMove = 'Y';
                } else {
                    yourMove = 'Z';
                }
            } else {
                if (opponent == 'A') {
                    yourMove = 'Y';
                } else if (opponent == 'B') {
                    yourMove = 'Z';
                } else {
                    yourMove = 'X';
                }
            }
            
            int score = 0;
            if (yourMove == 'X') {
                score = 1;
            } else if (yourMove == 'Y') {
                score = 2;
            } else if (yourMove == 'Z') {
                score = 3;
            }
            
            if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
                score += 6;
            } else if ((opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')) {
                score += 3;
            }
            
            totalScore += score;
        }
        
        printf("%d\n", totalScore);
    }
    return 0;
}