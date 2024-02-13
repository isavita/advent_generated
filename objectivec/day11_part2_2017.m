#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *steps = [input componentsSeparatedByString:@","];
        
        int maxDistance = 0;
        int x = 0;
        int y = 0;
        
        for (NSString *step in steps) {
            if ([step isEqualToString:@"n"]) {
                y++;
            } else if ([step isEqualToString:@"s"]) {
                y--;
            } else if ([step isEqualToString:@"ne"]) {
                x++;
                y++;
            } else if ([step isEqualToString:@"nw"]) {
                x--;
            y++;
            } else if ([step isEqualToString:@"se"]) {
                x++;
            y--;
            } else if ([step isEqualToString:@"sw"]) {
                x--;
            y--;
            }
            
            int distance = (abs(x) + abs(y) + abs(x + y)) / 2;
            if (distance > maxDistance) {
                maxDistance = distance;
            }
        }
        
        printf("Fewest number of steps required to reach the child process: %d\n", (abs(x) + abs(y) + abs(x + y)) / 2);
        printf("Furthest distance the child process ever got from the starting position: %d\n", maxDistance);
    }
    return 0;
}