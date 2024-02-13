#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSString *sequence = [input stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];

        for (int i = 0; i < 40; i++) {
            NSMutableString *newSequence = [NSMutableString string];
            char currentChar = [sequence characterAtIndex:0];
            int count = 1;

            for (int j = 1; j < sequence.length; j++) {
                if ([sequence characterAtIndex:j] == currentChar) {
                    count++;
                } else {
                    [newSequence appendFormat:@"%d%c", count, currentChar];
                    currentChar = [sequence characterAtIndex:j];
                    count = 1;
                }
            }

            [newSequence appendFormat:@"%d%c", count, currentChar];
            sequence = newSequence;
        }

        printf("%lu\n", (unsigned long)sequence.length);
    }
    return 0;
}