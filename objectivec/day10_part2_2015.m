#import <Foundation/Foundation.h>

NSArray<NSNumber *> *applyLookAndSay(NSArray<NSNumber *> *input) {
    NSMutableArray<NSNumber *> *result = [NSMutableArray array];
    NSNumber *currentDigit = input[0];
    NSInteger count = 1;
    
    for (NSInteger i = 1; i < input.count; i++) {
        if ([input[i] isEqualToNumber:currentDigit]) {
            count++;
        } else {
            [result addObject:@(count)];
            [result addObject:currentDigit];
            currentDigit = input[i];
            count = 1;
        }
    }
    
    [result addObject:@(count)];
    [result addObject:currentDigit];
    
    return result;
}

int main(int argc, char *argv[]) {
    NSString *inputPath = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
    NSString *input = [NSString stringWithContentsOfFile:inputPath encoding:NSUTF8StringEncoding error:nil];
    input = [input stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
    
    // Convert input string to array of integers
    NSMutableArray<NSNumber *> *part1Input = [NSMutableArray array];
    for (NSInteger i = 0; i < input.length; i++) {
        [part1Input addObject:@([input characterAtIndex:i] - '0')];
    }
    
    // Part 1
    for (NSInteger i = 0; i < 40; i++) {
        part1Input = applyLookAndSay(part1Input);
    }
    NSLog(@"Part 1 result length: %lu", (unsigned long)part1Input.count);
    
    // Part 2
    part1Input = [NSMutableArray array];
    for (NSInteger i = 0; i < input.length; i++) {
        [part1Input addObject:@([input characterAtIndex:i] - '0')];
    }
    for (NSInteger i = 0; i < 50; i++) {
        part1Input = applyLookAndSay(part1Input);
    }
    NSLog(@"Part 2 result length: %lu", (unsigned long)part1Input.count);
    
    return 0;
}