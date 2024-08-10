#import <Foundation/Foundation.h>

@interface Scratchcard : NSObject
@property (nonatomic, strong) NSArray<NSNumber *> *winningNumbers;
@property (nonatomic, strong) NSArray<NSNumber *> *yourNumbers;
- (instancetype)initWithWinningNumbers:(NSArray<NSNumber *> *)winningNumbers andYourNumbers:(NSArray<NSNumber *> *)yourNumbers;
- (NSInteger)calculatePoints;
- (NSInteger)countMatches;
@end

@implementation Scratchcard
- (instancetype)initWithWinningNumbers:(NSArray<NSNumber *> *)winningNumbers andYourNumbers:(NSArray<NSNumber *> *)yourNumbers {
    self = [super init];
    if (self) {
        _winningNumbers = winningNumbers;
        _yourNumbers = yourNumbers;
    }
    return self;
}

- (NSInteger)calculatePoints {
    NSInteger matches = [self countMatches];
    if (matches == 0) return 0;
    return pow(2, matches - 1);
}

- (NSInteger)countMatches {
    NSMutableSet *winningSet = [NSMutableSet setWithArray:_winningNumbers];
    NSInteger matches = 0;
    for (NSNumber *number in _yourNumbers) {
        if ([winningSet containsObject:number]) {
            matches++;
        }
    }
    return matches;
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];

        NSMutableArray *scratchcards = [NSMutableArray array];

        for (NSString *line in lines) {
            if ([line length] == 0) continue;
            NSArray *parts = [line componentsSeparatedByString:@":"];
            if ([parts count] < 2) continue;

            NSString *numbersPart = parts[1];
            NSArray *numberGroups = [numbersPart componentsSeparatedByString:@"|"];
            if ([numberGroups count] < 2) continue;

            NSArray *winningNumbers = [[numberGroups[0] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] componentsSeparatedByString:@" "];
            NSArray *yourNumbers = [[numberGroups[1] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] componentsSeparatedByString:@" "];

            NSMutableArray *winningNumbersArray = [NSMutableArray array];
            NSMutableArray *yourNumbersArray = [NSMutableArray array];

            for (NSString *number in winningNumbers) {
                if ([number length] > 0) {
                    [winningNumbersArray addObject:@([number integerValue])];
                }
            }

            for (NSString *number in yourNumbers) {
                if ([number length] > 0) {
                    [yourNumbersArray addObject:@([number integerValue])];
                }
            }

            Scratchcard *card = [[Scratchcard alloc] initWithWinningNumbers:winningNumbersArray andYourNumbers:yourNumbersArray];
            [scratchcards addObject:card];
        }

        // Part 1: Calculate total points
        NSInteger totalPoints = 0;
        for (Scratchcard *card in scratchcards) {
            totalPoints += [card calculatePoints];
        }
        NSLog(@"Total points: %ld", (long)totalPoints);

        // Part 2: Calculate total number of scratchcards
        NSMutableArray *cardCounts = [NSMutableArray arrayWithCapacity:[scratchcards count]];
        for (NSUInteger i = 0; i < [scratchcards count]; i++) {
            [cardCounts addObject:@(1)];
        }

        for (NSUInteger i = 0; i < [scratchcards count]; i++) {
            Scratchcard *card = scratchcards[i];
            NSInteger matches = [card countMatches];
            NSInteger currentCount = [cardCounts[i] integerValue];
            for (NSUInteger j = i + 1; j <= i + matches && j < [scratchcards count]; j++) {
                cardCounts[j] = @([cardCounts[j] integerValue] + currentCount);
            }
        }

        NSInteger totalCards = 0;
        for (NSNumber *count in cardCounts) {
            totalCards += [count integerValue];
        }
        NSLog(@"Total scratchcards: %ld", (long)totalCards);
    }
    return 0;
}