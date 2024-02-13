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
            
            NSCharacterSet *vowels = [NSCharacterSet characterSetWithCharactersInString:@"aeiou"];
            NSCharacterSet *doubleLetters = [NSCharacterSet characterSetWithCharactersInString:@"abcdefghijklmnopqrstuvwxyz"];
            BOOL hasThreeVowels = [[string componentsSeparatedByCharactersInSet:vowels] count] >= 4;
            BOOL hasDoubleLetter = NO;
            
            for (int i = 0; i < string.length - 1; i++) {
                if ([string characterAtIndex:i] == [string characterAtIndex:i + 1]) {
                    hasDoubleLetter = YES;
                    break;
                }
            }
            
            if (hasThreeVowels && hasDoubleLetter) {
                niceCount++;
            }
        }
        
        printf("%d\n", niceCount);
    }
    return 0;
}