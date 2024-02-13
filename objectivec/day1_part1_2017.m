
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        input = [input stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        
        int sum = 0;
        for (int i = 0; i < input.length; i++) {
            int currentDigit = [input characterAtIndex:i] - '0';
            int nextDigit = [input characterAtIndex:(i + 1) % input.length] - '0';
            
            if (currentDigit == nextDigit) {
                sum += currentDigit;
            }
        }
        
        printf("%d\n", sum);
    }
    return 0;
}
