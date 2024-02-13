#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        
        int floor = 0;
        for (int i = 0; i < input.length; i++) {
            unichar c = [input characterAtIndex:i];
            if (c == '(') {
                floor++;
            } else if (c == ')') {
                floor--;
            }
        }
        
        printf("Santa ends up on floor %d\n", floor);
    }
    return 0;
}