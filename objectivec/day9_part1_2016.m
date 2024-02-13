#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        
        NSString *cleanedInput = [input stringByReplacingOccurrencesOfString:@" " withString:@""];
        
        NSUInteger decompressedLength = 0;
        NSUInteger currentIndex = 0;
        
        while (currentIndex < cleanedInput.length) {
            if ([cleanedInput characterAtIndex:currentIndex] == '(') {
                NSRange markerRange = [cleanedInput rangeOfString:@"(" options:0 range:NSMakeRange(currentIndex, cleanedInput.length - currentIndex)];
                NSRange closingParenthesisRange = [cleanedInput rangeOfString:@")" options:0 range:NSMakeRange(markerRange.location, cleanedInput.length - markerRange.location)];
                
                NSString *marker = [cleanedInput substringWithRange:NSMakeRange(markerRange.location + 1, closingParenthesisRange.location - markerRange.location - 1)];
                NSArray *parts = [marker componentsSeparatedByString:@"x"];
                
                NSInteger charactersToRepeat = [parts[0] integerValue];
                NSInteger repeatCount = [parts[1] integerValue];
                
                NSRange dataRange = NSMakeRange(closingParenthesisRange.location + 1, charactersToRepeat);
                NSString *dataToRepeat = [cleanedInput substringWithRange:dataRange];
                
                decompressedLength += charactersToRepeat * repeatCount;
                
                currentIndex = dataRange.location + charactersToRepeat;
            } else {
                decompressedLength++;
                currentIndex++;
            }
        }
        
        printf("Decompressed length: %lu\n", decompressedLength);
    }
    return 0;
}