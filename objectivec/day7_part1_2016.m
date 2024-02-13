#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *ips = [input componentsSeparatedByString:@"\n"];
        
        int count = 0;
        for (NSString *ip in ips) {
            BOOL supportsTLS = YES;
            BOOL inHypernet = NO;
            BOOL abbaFound = NO;
            NSMutableArray *hypernetSequences = [NSMutableArray array];
            NSMutableString *currentSequence = [NSMutableString string];
            
            for (int i = 0; i < ip.length; i++) {
                unichar c = [ip characterAtIndex:i];
                
                if (c == '[') {
                    inHypernet = YES;
                    [hypernetSequences addObject:currentSequence];
                    currentSequence = [NSMutableString string];
                } else if (c == ']') {
                    inHypernet = NO;
                    [hypernetSequences addObject:currentSequence];
                    currentSequence = [NSMutableString string];
                } else {
                    [currentSequence appendFormat:@"%c", c];
                }
                
                if (currentSequence.length >= 4) {
                    for (int j = 0; j < currentSequence.length - 3; j++) {
                        NSString *sub = [currentSequence substringWithRange:NSMakeRange(j, 4)];
                        if ([sub characterAtIndex:0] == [sub characterAtIndex:3] &&
                            [sub characterAtIndex:1] == [sub characterAtIndex:2] &&
                            [sub characterAtIndex:0] != [sub characterAtIndex:1]) {
                            if (inHypernet) {
                                supportsTLS = NO;
                            } else {
                                abbaFound = YES;
                            }
                        }
                    }
                }
            }
            
            if (abbaFound && supportsTLS) {
                count++;
            }
        }
        
        printf("%d\n", count);
    }
    return 0;
}