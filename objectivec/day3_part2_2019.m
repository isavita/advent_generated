
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *paths = [input componentsSeparatedByString:@"\n"];
        NSArray *wire1 = [paths[0] componentsSeparatedByString:@","];
        NSArray *wire2 = [paths[1] componentsSeparatedByString:@","];
        
        NSMutableDictionary *grid = [NSMutableDictionary dictionary];
        int minDistance = INT_MAX;
        int minSteps = INT_MAX;
        
        int x = 0, y = 0, steps = 0;
        for (NSString *move in wire1) {
            char direction = [move characterAtIndex:0];
            int distance = [[move substringFromIndex:1] intValue];
            for (int i = 0; i < distance; i++) {
                steps++;
                switch (direction) {
                    case 'R':
                        x++;
                        break;
                    case 'L':
                        x--;
                        break;
                    case 'U':
                        y++;
                        break;
                    case 'D':
                        y--;
                        break;
                }
                NSString *key = [NSString stringWithFormat:@"%d,%d", x, y];
                if (grid[key] == nil) {
                    grid[key] = @(steps);
                }
            }
        }
        
        x = 0;
        y = 0;
        steps = 0;
        for (NSString *move in wire2) {
            char direction = [move characterAtIndex:0];
            int distance = [[move substringFromIndex:1] intValue];
            for (int i = 0; i < distance; i++) {
                steps++;
                switch (direction) {
                    case 'R':
                        x++;
                        break;
                    case 'L':
                        x--;
                        break;
                    case 'U':
                        y++;
                        break;
                    case 'D':
                        y--;
                        break;
                }
                NSString *key = [NSString stringWithFormat:@"%d,%d", x, y];
                if (grid[key] != nil) {
                    int distance = abs(x) + abs(y);
                    minDistance = MIN(minDistance, distance);
                    minSteps = MIN(minSteps, [grid[key] intValue] + steps);
                }
            }
        }
        
        printf("Manhattan distance: %d\n", minDistance);
        printf("Fewest combined steps: %d\n", minSteps);
    }
    return 0;
}
