#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        int x = 0;
        int y = 0;
        NSMutableSet *houses = [NSMutableSet set];
        [houses addObject:[NSString stringWithFormat:@"%d,%d", x, y]];
        
        for (int i = 0; i < input.length; i++) {
            unichar direction = [input characterAtIndex:i];
            if (direction == '^') {
                y++;
            } else if (direction == 'v') {
                y--;
            } else if (direction == '>') {
                x++;
            } else if (direction == '<') {
                x--;
            }
            [houses addObject:[NSString stringWithFormat:@"%d,%d", x, y]];
        }
        
        printf("%lu\n", houses.count);
    }
    return 0;
}