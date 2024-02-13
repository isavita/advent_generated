#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *steps = [input componentsSeparatedByString:@","];
        
        int sum = 0;
        for (NSString *step in steps) {
            int current = 0;
            for (int i = 0; i < step.length; i++) {
                int ascii = [step characterAtIndex:i];
                current += ascii;
                current *= 17;
                current %= 256;
            }
            sum += current;
        }
        
        printf("%d\n", sum);
    }
    return 0;
}