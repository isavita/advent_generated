#import <Foundation/Foundation.h>

NSArray *parseInput(NSArray *input) {
    NSMutableArray *histories = [NSMutableArray array];
    for (NSString *line in input) {
        NSArray *numbers = [[line componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] valueForKey:@"intValue"];
        [histories addObject:numbers];
    }
    return histories;
}

BOOL allZeros(NSArray *nums) {
    for (NSNumber *num in nums) {
        if ([num intValue] != 0) return NO;
    }
    return YES;
}

NSArray *calculateExtrapolation(NSArray *history) {
    NSMutableArray *extrapolations = [NSMutableArray array];
    for (NSUInteger i = 1; i < history.count; i++) {
        [extrapolations addObject:@([history[i] intValue] - [history[i-1] intValue])];
    }
    return extrapolations;
}

NSArray *calculateExtrapolations(NSArray *history) {
    NSMutableArray *extrapolationsSeries = [NSMutableArray arrayWithObject:history];
    for (NSUInteger i = 1; i < history.count; i++) {
        NSArray *previousExtrapolations = extrapolationsSeries[i-1];
        if (allZeros(previousExtrapolations)) return extrapolationsSeries;

        NSArray *extrapolations = calculateExtrapolation(previousExtrapolations);
        [extrapolationsSeries addObject:extrapolations];
    }
    return extrapolationsSeries;
}

int solve(NSArray *input) {
    NSArray *histories = parseInput(input);
    int res = 0;

    for (NSArray *history in histories) {
        NSArray *extrapolationsSeries = calculateExtrapolations(history);
        int pastPrediction = 0;
        for (NSInteger i = extrapolationsSeries.count - 1; i >= 0; i--) {
            pastPrediction = [extrapolationsSeries[i][0] intValue] - pastPrediction;
        }
        res += pastPrediction;
    }
    return res;
}

NSArray *readFile(NSString *fileName) {
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:fileName encoding:NSUTF8StringEncoding error:&error];
    if (error) @throw error;
    return [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray *input = readFile(@"input.txt");
        NSLog(@"%d", solve(input));
    }
    return 0;
}