#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *instructions = [input componentsSeparatedByString:@"\n"];
        
        int deckSize = 10007;
        int card = 2019;
        
        for (NSString *instruction in instructions) {
            if ([instruction isEqualToString:@"deal into new stack"]) {
                card = deckSize - 1 - card;
            } else if ([instruction hasPrefix:@"cut"]) {
                int n = [[instruction componentsSeparatedByString:@" "] lastObject].intValue;
                card = (card - n + deckSize) % deckSize;
            } else if ([instruction hasPrefix:@"deal with increment"]) {
                int n = [[instruction componentsSeparatedByString:@" "] lastObject].intValue;
                card = (card * n) % deckSize;
            }
        }
        
        printf("%d\n", card);
    }
    return 0;
}