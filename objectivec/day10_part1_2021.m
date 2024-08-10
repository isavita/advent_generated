#import <Foundation/Foundation.h>

NSDictionary<NSString*, NSNumber*> *errorScores = @{
    @")": @3,
    @"]": @57,
    @"}": @1197,
    @">": @25137
};

NSDictionary<NSString*, NSString*> *matchingBrackets = @{
    @"(": @")",
    @"[": @"]",
    @"{": @"}",
    @"<": @">"
};

NSString *filePath = @"input.txt";

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (!fileContent) {
            NSLog(@"Error reading file: %@", error.localizedDescription);
            return 1;
        }

        NSArray *lines = [fileContent componentsSeparatedByString:@"\n"];
        int totalScore = 0;

        for (NSString *line in lines) {
            NSMutableArray<NSString*> *stack = [NSMutableArray array];
            for (NSUInteger i = 0; i < line.length; i++) {
                NSString *charStr = [line substringWithRange:NSMakeRange(i, 1)];
                if ([matchingBrackets.allKeys containsObject:charStr]) {
                    [stack addObject:charStr];
                } else {
                    NSString *lastOpenBracket = stack.lastObject;
                    NSString *expectedCloseBracket = matchingBrackets[lastOpenBracket];
                    if (![expectedCloseBracket isEqualToString:charStr]) {
                        totalScore += [errorScores[charStr] intValue];
                        break;
                    } else {
                        [stack removeLastObject];
                    }
                }
            }
        }

        NSLog(@"Total Syntax Error Score: %d", totalScore);
    }
    return 0;
}