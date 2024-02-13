#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *grid = [NSMutableDictionary dictionary];
        
        for (NSString *line in lines) {
            NSArray *coords = [line componentsSeparatedByString:@" -> "];
            NSArray *start = [coords[0] componentsSeparatedByString:@","];
            NSArray *end = [coords[1] componentsSeparatedByString:@","];
            
            int x1 = [start[0] intValue];
            int y1 = [start[1] intValue];
            int x2 = [end[0] intValue];
            int y2 = [end[1] intValue];
            
            if (x1 == x2) {
                for (int y = MIN(y1, y2); y <= MAX(y1, y2); y++) {
                    NSString *key = [NSString stringWithFormat:@"%d,%d", x1, y];
                    if (grid[key]) {
                        grid[key] = @([grid[key] intValue] + 1);
                    } else {
                        grid[key] = @1;
                    }
                }
            } else if (y1 == y2) {
                for (int x = MIN(x1, x2); x <= MAX(x1, x2); x++) {
                    NSString *key = [NSString stringWithFormat:@"%d,%d", x, y1];
                    if (grid[key]) {
                        grid[key] = @([grid[key] intValue] + 1);
                    } else {
                        grid[key] = @1;
                    }
                }
            }
        }
        
        int count = 0;
        for (NSString *key in grid) {
            if ([grid[key] intValue] >= 2) {
                count++;
            }
        }
        
        printf("%d\n", count);
    }
    return 0;
}