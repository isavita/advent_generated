#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSInteger sum = 0;
        
        for (int i = 0; i < input.length; i++) {
            if ([input characterAtIndex:i] == [input characterAtIndex:(i + input.length/2) % input.length]) {
                sum += [[NSString stringWithFormat:@"%c", [input characterAtIndex:i]] integerValue];
            }
        }
        
        printf("%ld\n", sum);
    }
    return 0;
}