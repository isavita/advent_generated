#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@", "];
        
        int x = 0;
        int y = 0;
        int direction = 0; // 0 - North, 1 - East, 2 - South, 3 - West
        NSMutableSet *visitedLocations = [NSMutableSet set];
        BOOL foundTwice = NO;
        
        for (NSString *instruction in instructions) {
            NSString *turn = [instruction substringToIndex:1];
            int distance = [[instruction substringFromIndex:1] intValue];
            
            if ([turn isEqualToString:@"R"]) {
                direction = (direction + 1) % 4;
            } else {
                direction = (direction + 3) % 4;
            }
            
            for (int i = 0; i < distance; i++) {
                if (direction == 0) {
                    y++;
                } else if (direction == 1) {
                    x++;
                } else if (direction == 2) {
                    y--;
                } else {
                    x--;
                }
                
                NSString *location = [NSString stringWithFormat:@"%d,%d", x, y];
                if ([visitedLocations containsObject:location] && !foundTwice) {
                    printf("Part Two: %d\n", abs(x) + abs(y));
                    foundTwice = YES;
                }
                
                [visitedLocations addObject:location];
            }
        }
        
        printf("Part One: %d\n", abs(x) + abs(y));
    }
    return 0;
}