#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *strings = [input componentsSeparatedByString:@"\n"];
        
        int niceCount = 0;
        
        for (NSString *string in strings) {
            if ([string rangeOfString:@"ab"].location != NSNotFound || [string rangeOfString:@"cd"].location != NSNotFound || [string rangeOfString:@"pq"].location != NSNotFound || [string rangeOfString:@"xy"].location != NSNotFound) {
                continue;
            }
            
            NSRegularExpression *vowelsRegex = [NSRegularExpression regularExpressionWithPattern:@"[aeiou]" options:0 error:nil];
            NSUInteger vowelsCount = [vowelsRegex numberOfMatchesInString:string options:0 range:NSMakeRange(0, [string length])];
            
            NSRegularExpression *doubleLetterRegex = [NSRegularExpression regularExpressionWithPattern:@"([a-z])\\1" options:0 error:nil];
            NSUInteger doubleLetterCount = [doubleLetterRegex numberOfMatchesInString:string options:0 range:NSMakeRange(0, [string length])];
            
            if (vowelsCount >= 3 && doubleLetterCount >= 1) {
                niceCount++;
            }
        }
        
        printf("%d\n", niceCount);
    }
    return 0;
}