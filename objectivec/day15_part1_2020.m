#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *startingNumbers = [fileContent componentsSeparatedByString:@","];

        NSMutableDictionary *lastSpokenTurns = [NSMutableDictionary dictionary];
        NSMutableArray *spokenNumbers = [NSMutableArray array];

        // Parse starting numbers and initialize the spokenNumbers array
        for (NSString *numberStr in startingNumbers) {
            NSInteger number = [numberStr integerValue];
            [spokenNumbers addObject:@(number)];
            [lastSpokenTurns setObject:@([spokenNumbers count]) forKey:@(number)];
        }

        // Play the game until the 2020th number is spoken
        for (NSInteger turn = [spokenNumbers count] + 1; turn <= 2020; turn++) {
            NSInteger lastNumber = [[spokenNumbers lastObject] integerValue];
            NSInteger numberToSpeak;

            if ([lastSpokenTurns objectForKey:@(lastNumber)] == nil) {
                numberToSpeak = 0;
            } else {
                NSInteger previousTurn = [[lastSpokenTurns objectForKey:@(lastNumber)] integerValue];
                numberToSpeak = turn - 1 - previousTurn;
            }

            [spokenNumbers addObject:@(numberToSpeak)];
            [lastSpokenTurns setObject:@(turn - 1) forKey:@(lastNumber)];
        }

        // Print the 2020th number spoken
        NSInteger result = [[spokenNumbers lastObject] integerValue];
        NSLog(@"The 2020th number spoken is: %ld", (long)result);
    }
    return 0;
}