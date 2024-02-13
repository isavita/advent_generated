#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        int floor = 0;
        int position = 0;
        BOOL basementFound = NO;
        
        for (int i = 0; i < input.length; i++) {
            unichar instruction = [input characterAtIndex:i];
            if (instruction == '(') {
                floor++;
            } else if (instruction == ')') {
                floor--;
            }
            
            if (floor == -1 && !basementFound) {
                position = i + 1;
                basementFound = YES;
            }
        }
        
        printf("Santa ends up on floor %d\n", floor);
        printf("Position of the character that causes Santa to first enter the basement: %d\n", position);
    }
    return 0;
}