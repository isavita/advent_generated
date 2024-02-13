#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        input = [input stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        
        NSMutableString *polymer = [input mutableCopy];
        
        BOOL reacted = YES;
        while (reacted) {
            reacted = NO;
            for (int i = 0; i < polymer.length - 1; i++) {
                char unit1 = [polymer characterAtIndex:i];
                char unit2 = [polymer characterAtIndex:i + 1];
                
                if (abs(unit1 - unit2) == 32) {
                    [polymer deleteCharactersInRange:NSMakeRange(i, 2)];
                    reacted = YES;
                    break;
                }
            }
        }
        
        printf("%lu\n", polymer.length);
    }
    return 0;
}