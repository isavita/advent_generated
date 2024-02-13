#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *paths = [input componentsSeparatedByString:@"\n"];
        
        NSArray *path1 = [paths[0] componentsSeparatedByString:@","];
        NSArray *path2 = [paths[1] componentsSeparatedByString:@","];
        
        NSMutableDictionary *grid = [NSMutableDictionary dictionary];
        
        int x = 0;
        int y = 0;
        int steps = 0;
        
        for (NSString *move in path1) {
            unichar direction = [move characterAtIndex:0];
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
                grid[key] = @(steps);
            }
        }
        
        x = 0;
        y = 0;
        steps = 0;
        int minDistance = INT_MAX;
        
        for (NSString *move in path2) {
            unichar direction = [move characterAtIndex:0];
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
                if (grid[key]) {
                    int distance = abs(x) + abs(y);
                    minDistance = MIN(minDistance, distance);
                }
            }
        }
        
        printf("%d\n", minDistance);
    }
    return 0;
}