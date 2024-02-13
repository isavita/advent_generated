#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        char line[100];
        NSMutableArray *points = [NSMutableArray array];
        int maxX = 0, maxY = 0;
        while (fgets(line, 100, file)) {
            NSString *coords = [NSString stringWithUTF8String:line];
            NSArray *coordArray = [coords componentsSeparatedByString:@", "];
            int x = [coordArray[0] intValue];
            int y = [coordArray[1] intValue];
            if (x > maxX) {
                maxX = x;
            }
            if (y > maxY) {
                maxY = y;
            }
            [points addObject:@[@(x), @(y)]];
        }
        fclose(file);
        
        NSMutableArray *grid = [NSMutableArray array];
        for (int i = 0; i < maxX+2; i++) {
            NSMutableArray *row = [NSMutableArray array];
            for (int j = 0; j < maxY+2; j++) {
                [row addObject:@(0)];
            }
            [grid addObject:row];
        }
        
        NSMutableArray *areas = [NSMutableArray arrayWithCapacity:points.count];
        NSMutableArray *infinite = [NSMutableArray arrayWithCapacity:points.count];
        for (int i = 0; i < points.count; i++) {
            [areas addObject:@(0)];
            [infinite addObject:@(NO)];
        }
        
        for (int i = 0; i < grid.count; i++) {
            for (int j = 0; j < [grid[i] count]; j++) {
                int minDist = maxX + maxY;
                for (int k = 0; k < points.count; k++) {
                    NSArray *point = points[k];
                    int dist = abs([point[0] intValue] - i) + abs([point[1] intValue] - j);
                    if (dist < minDist) {
                        minDist = dist;
                        grid[i][j] = @(k);
                    } else if (dist == minDist) {
                        grid[i][j] = @(-1);
                    }
                }
                if ([grid[i][j] intValue] != -1) {
                    if (i == 0 || j == 0 || i == maxX+1 || j == maxY+1) {
                        infinite[[grid[i][j] intValue]] = @(YES);
                    }
                    areas[[grid[i][j] intValue]] = @([areas[[grid[i][j] intValue]] intValue] + 1);
                }
            }
        }
        
        int maxArea = 0;
        for (int i = 0; i < areas.count; i++) {
            if (![infinite[i] boolValue] && [areas[i] intValue] > maxArea) {
                maxArea = [areas[i] intValue];
            }
        }
        
        printf("%d\n", maxArea);
    }
    return 0;
}