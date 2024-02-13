#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        int totalScore = 0;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            
            NSArray *round = [line componentsSeparatedByString:@" "];
            NSString *opponent = round[0];
            NSString *response = round[1];
            
            int opponentScore = 0;
            int responseScore = 0;
            
            if ([opponent isEqualToString:@"A"]) {
                opponentScore = 1;
            } else if ([opponent isEqualToString:@"B"]) {
                opponentScore = 2;
            } else if ([opponent isEqualToString:@"C"]) {
                opponentScore = 3;
            }
            
            if ([response isEqualToString:@"X"]) {
                responseScore = 1;
            } else if ([response isEqualToString:@"Y"]) {
                responseScore = 2;
            } else if ([response isEqualToString:@"Z"]) {
                responseScore = 3;
            }
            
            int roundScore = 0;
            if ((opponentScore == 1 && responseScore == 2) || (opponentScore == 2 && responseScore == 3) || (opponentScore == 3 && responseScore == 1)) {
                roundScore = 6;
            } else if ((opponentScore == 1 && responseScore == 1) || (opponentScore == 2 && responseScore == 2) || (opponentScore == 3 && responseScore == 3)) {
                roundScore = 3;
            } else {
                roundScore = 0;
            }
            
            totalScore += (responseScore + roundScore);
        }
        
        printf("%d\n", totalScore);
    }
    return 0;
}