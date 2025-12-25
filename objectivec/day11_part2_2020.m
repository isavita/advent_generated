
#import <Foundation/Foundation.h>

static const int dirs[8][2] = {{-1,-1},{0,-1},{1,-1},{-1,0},{1,0},{-1,1},{0,1},{1,1}};

int countVisible(NSArray<NSMutableString *> *grid, int r, int c) {
    int rows = (int)grid.count, cols = (int)[grid[0] length], cnt = 0;
    for (int d = 0; d < 8; ++d) {
        int dr = dirs[d][1], dc = dirs[d][0];
        int rr = r + dr, cc = c + dc;
        while (rr >= 0 && rr < rows && cc >= 0 && cc < cols) {
            unichar ch = [grid[rr] characterAtIndex:cc];
            if (ch == 'L') break;
            if (ch == '#') { ++cnt; break; }
            rr += dr; cc += dc;
        }
    }
    return cnt;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:&err];
        if (!txt) { NSLog(@"%@", err); return 1; }
        NSMutableArray<NSMutableString *> *grid = [NSMutableArray array];
        for (NSString *line in [txt componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]]) {
            if (line.length) [grid addObject:[line mutableCopy]];
        }

        BOOL changed = YES;
        while (changed) {
            changed = NO;
            NSMutableArray<NSMutableString *> *next = [NSMutableArray arrayWithCapacity:grid.count];
            for (NSMutableString *row in grid) [next addObject:[row mutableCopy]];
            int rows = (int)grid.count, cols = (int)[grid[0] length];
            for (int i = 0; i < rows; ++i) {
                for (int j = 0; j < cols; ++j) {
                    unichar seat = [grid[i] characterAtIndex:j];
                    if (seat == 'L' && countVisible(grid, i, j) == 0) {
                        [next[i] replaceCharactersInRange:NSMakeRange(j, 1) withString:@"#"];
                        changed = YES;
                    } else if (seat == '#' && countVisible(grid, i, j) >= 5) {
                        [next[i] replaceCharactersInRange:NSMakeRange(j, 1) withString:@"L"];
                        changed = YES;
                    }
                }
            }
            grid = next;
        }

        int occupied = 0;
        for (NSMutableString *row in grid) {
            for (NSUInteger i = 0; i < row.length; ++i)
                if ([row characterAtIndex:i] == '#') ++occupied;
        }
        printf("%d\n", occupied);
    }
    return 0;
}
