#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        
        NSMutableSet<NSString *> *points = [NSMutableSet set];
        NSMutableArray<NSArray<NSNumber *> *> *folds = [NSMutableArray array];
        BOOL readingPoints = YES;
        
        for (NSString *line in lines) {
            NSString *trimmed = [line stringByTrimmingCharactersInSet:
                                 [NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if ([trimmed length] == 0) {
                readingPoints = NO;
                continue;
            }
            if (readingPoints) {
                [points addObject:trimmed];
            } else {
                NSArray *parts = [trimmed componentsSeparatedByString:@"="];
                NSString *axis = parts[0];
                int value = [parts[1] intValue];
                if ([axis hasSuffix:@"x"]) {
                    [folds addObject:@[@(value), @0]];
                } else {
                    [folds addObject:@[@0, @(value)]];
                }
            }
        }
        
        for (NSUInteger i = 0; i < folds.count; i++) {
            NSArray<NSNumber *> *fold = folds[i];
            int fx = [fold[0] intValue];
            int fy = [fold[1] intValue];
            NSMutableSet<NSString *> *newPoints = [NSMutableSet set];
            for (NSString *pt in points) {
                NSArray *xy = [pt componentsSeparatedByString:@","];
                int x = [xy[0] intValue];
                int y = [xy[1] intValue];
                if (fx && x > fx) x = fx - (x - fx);
                if (fy && y > fy) y = fy - (y - fy);
                [newPoints addObject:[NSString stringWithFormat:@"%d,%d", x, y]];
            }
            points = newPoints;
            if (i == 0) {
                printf("Number of dots visible after first fold: %lu\n", (unsigned long)points.count);
            }
        }
        
        int maxX = 0, maxY = 0;
        for (NSString *pt in points) {
            NSArray *xy = [pt componentsSeparatedByString:@","];
            int x = [xy[0] intValue];
            int y = [xy[1] intValue];
            if (x > maxX) maxX = x;
            if (y > maxY) maxY = y;
        }
        
        char grid[maxY + 1][maxX + 2];
        memset(grid, ' ', sizeof(grid));
        for (int y = 0; y <= maxY; y++) grid[y][maxX + 1] = '\0';
        
        for (NSString *pt in points) {
            NSArray *xy = [pt componentsSeparatedByString:@","];
            int x = [xy[0] intValue];
            int y = [xy[1] intValue];
            grid[y][x] = '#';
        }
        
        for (int y = 0; y <= maxY; y++) {
            printf("%s\n", grid[y]);
        }
    }
    return 0;
}