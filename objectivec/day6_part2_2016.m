#import <Foundation/Foundation.h>

@interface SignalDecoder : NSObject
- (NSString *)mostCommonCharactersInColumns:(NSArray<NSString *> *)messages;
- (NSString *)leastCommonCharactersInColumns:(NSArray<NSString *> *)messages;
@end

@implementation SignalDecoder

- (NSString *)mostCommonCharactersInColumns:(NSArray<NSString *> *)messages {
    if (messages.count == 0) return @"";

    NSUInteger messageLength = [messages[0] length];
    NSMutableArray<NSMutableDictionary *> *frequencyCounts = [NSMutableArray arrayWithCapacity:messageLength];
    for (NSUInteger i = 0; i < messageLength; i++) {
        frequencyCounts[i] = [NSMutableDictionary dictionary];
    }

    for (NSString *message in messages) {
        for (NSUInteger i = 0; i < messageLength; i++) {
            unichar charAtIndex = [message characterAtIndex:i];
            NSString *charStr = [NSString stringWithCharacters:&charAtIndex length:1];
            NSMutableDictionary *freqDict = frequencyCounts[i];
            NSNumber *count = freqDict[charStr];
            freqDict[charStr] = @(count.integerValue + 1);
        }
    }

    NSMutableString *result = [NSMutableString stringWithCapacity:messageLength];
    for (NSUInteger i = 0; i < messageLength; i++) {
        NSMutableDictionary *freqDict = frequencyCounts[i];
        NSString *mostCommonChar = nil;
        NSInteger maxCount = -1;

        for (NSString *charStr in freqDict) {
            NSInteger count = [freqDict[charStr] integerValue];
            if (count > maxCount) {
                maxCount = count;
                mostCommonChar = charStr;
            }
        }

        [result appendString:mostCommonChar];
    }

    return result;
}

- (NSString *)leastCommonCharactersInColumns:(NSArray<NSString *> *)messages {
    if (messages.count == 0) return @"";

    NSUInteger messageLength = [messages[0] length];
    NSMutableArray<NSMutableDictionary *> *frequencyCounts = [NSMutableArray arrayWithCapacity:messageLength];
    for (NSUInteger i = 0; i < messageLength; i++) {
        frequencyCounts[i] = [NSMutableDictionary dictionary];
    }

    for (NSString *message in messages) {
        for (NSUInteger i = 0; i < messageLength; i++) {
            unichar charAtIndex = [message characterAtIndex:i];
            NSString *charStr = [NSString stringWithCharacters:&charAtIndex length:1];
            NSMutableDictionary *freqDict = frequencyCounts[i];
            NSNumber *count = freqDict[charStr];
            freqDict[charStr] = @(count.integerValue + 1);
        }
    }

    NSMutableString *result = [NSMutableString stringWithCapacity:messageLength];
    for (NSUInteger i = 0; i < messageLength; i++) {
        NSMutableDictionary *freqDict = frequencyCounts[i];
        NSString *leastCommonChar = nil;
        NSInteger minCount = INT_MAX;

        for (NSString *charStr in freqDict) {
            NSInteger count = [freqDict[charStr] integerValue];
            if (count < minCount) {
                minCount = count;
                leastCommonChar = charStr;
            }
        }

        [result appendString:leastCommonChar];
    }

    return result;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];

        if (error) {
            NSLog(@"Error reading file: %@", error.localizedDescription);
            return 1;
        }

        NSArray<NSString *> *messages = [fileContents componentsSeparatedByString:@"\n"];

        SignalDecoder *decoder = [[SignalDecoder alloc] init];
        NSString *mostCommonMessage = [decoder mostCommonCharactersInColumns:messages];
        NSString *leastCommonMessage = [decoder leastCommonCharactersInColumns:messages];

        NSLog(@"Most common message: %@", mostCommonMessage);
        NSLog(@"Least common message: %@", leastCommonMessage);
    }
    return 0;
}