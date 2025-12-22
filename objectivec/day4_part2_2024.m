
#import <Foundation/Foundation.h>

static BOOL checkMAS(NSArray<NSString *> *grid, NSInteger rows, NSInteger cols,
                     NSInteger x, NSInteger y, NSInteger dx, NSInteger dy) {
    if (x < 0 || y < 0 || x >= rows || y >= cols) return NO;
    const char word[3] = {'M','A','S'};
    BOOL forward = YES, backward = YES;
    for (NSInteger i = 0; i < 3; i++) {
        NSInteger nx = x + dx * i, ny = y + dy * i;
        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols) {
            forward = NO; backward = NO; break;
        }
        unichar c = [grid[nx] characterAtIndex:ny];
        if (c != word[i]) forward = NO;
        if (c != word[2 - i]) backward = NO;
    }
    return forward || backward;
}

static BOOL checkXMAS(NSArray<NSString *> *grid, NSInteger rows, NSInteger cols,
                      NSInteger x, NSInteger y) {
    return (checkMAS(grid, rows, cols, x-1, y-1, 1, 1) &&
            checkMAS(grid, rows, cols, x-1, y+1, 1, -1)) ||
           (checkMAS(grid, rows, cols, x+1, y-1, -1, 1) &&
            checkMAS(grid, rows, cols, x+1, y+1, -1, -1));
}

static NSInteger countXMASPatterns(NSArray<NSString *> *grid,
                                   NSInteger rows, NSInteger cols) {
    if (rows < 3 || cols < 3) return 0;
    NSInteger cnt = 0;
    for (NSInteger i = 1; i < rows-1; i++) {
        for (NSInteger j = 1; j < cols-1; j++) {
            if ([grid[i] characterAtIndex:j] == 'A' && checkXMAS(grid, rows, cols, i, j))
                cnt++;
        }
    }
    return cnt;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:&err];
        if (!data) { perror("Error opening file"); return 1; }
        NSMutableArray<NSString *> *grid = [NSMutableArray array];
        for (NSString *line in [data componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]]) {
            if (line.length) [grid addObject:line];
        }
        NSInteger rows = grid.count;
        NSInteger cols = rows ? grid[0].length : 0;
        NSInteger count = countXMASPatterns(grid, rows, cols);
        printf("X-MAS patterns appear %ld times in the word search\n", (long)count);
    }
    return 0;
}
