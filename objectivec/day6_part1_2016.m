#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        NSInteger messageLength = [lines[0] length];
        NSMutableArray *message = [NSMutableArray arrayWithCapacity:messageLength];
        
        for (NSInteger i = 0; i < messageLength; i++) {
            NSMutableDictionary *charCount = [NSMutableDictionary dictionary];
            for (NSString *line in lines) {
                NSString *currentChar = [line substringWithRange:NSMakeRange(i, 1)];
                NSNumber *count = charCount[currentChar];
                if (count) {
                    charCount[currentChar] = @(count.integerValue + 1);
                } else {
                    charCount[currentChar] = @1;
                }
            }
            
            NSString *mostCommonChar = nil;
            NSInteger maxCount = 0;
            for (NSString *key in charCount) {
                NSInteger count = [charCount[key] integerValue];
                if (count > maxCount) {
                    mostCommonChar = key;
                    maxCount = count;
                }
            }
            
            [message addObject:mostCommonChar];
        }
        
        printf("%s\n", [[message componentsJoinedByString:@""] UTF8String]);
    }
    return 0;
}