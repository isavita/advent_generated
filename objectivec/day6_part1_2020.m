#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *groups = [input componentsSeparatedByString:@"\n\n"];
        
        int sum = 0;
        for (NSString *group in groups) {
            NSCharacterSet *charSet = [NSCharacterSet characterSetWithCharactersInString:@"abcdefghijklmnopqrstuvwxyz"];
            NSCountedSet *countedSet = [[NSCountedSet alloc] init];
            NSArray *lines = [group componentsSeparatedByString:@"\n"];
            for (NSString *line in lines) {
                NSScanner *scanner = [NSScanner scannerWithString:line];
                while (![scanner isAtEnd]) {
                    NSString *answer = @"";
                    [scanner scanCharactersFromSet:charSet intoString:&answer];
                    for (int i = 0; i < answer.length; i++) {
                        [countedSet addObject:[NSString stringWithFormat:@"%c", [answer characterAtIndex:i]]];
                    }
                }
            }
            sum += countedSet.count;
        }
        
        printf("Sum of counts: %d\n", sum);
    }
    return 0;
}