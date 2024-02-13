#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@", "];
        
        int x = 0;
        int y = 0;
        int direction = 0; // 0: North, 1: East, 2: South, 3: West
        
        for (NSString *instruction in instructions) {
            unichar turn = [instruction characterAtIndex:0];
            int distance = [[instruction substringFromIndex:1] intValue];
            
            if (turn == 'R') {
                direction = (direction + 1) % 4;
            } else {
                direction = (direction + 3) % 4;
            }
            
            if (direction == 0) {
                y += distance;
            } else if (direction == 1) {
                x += distance;
            } else if (direction == 2) {
                y -= distance;
            } else {
                x -= distance;
            }
        }
        
        int distance = abs(x) + abs(y);
        printf("%d\n", distance);
    }
    return 0;
}