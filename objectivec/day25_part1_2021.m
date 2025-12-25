#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                       encoding:NSUTF8StringEncoding
                                                          error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        
        NSMutableArray<NSMutableString*> *grid = [NSMutableArray array];
        for (NSString *line in lines) {
            if (line.length) [grid addObject:[line mutableCopy]];
        }
        
        int steps = 0;
        int rows = (int)grid.count;
        int cols = rows ? (int)[grid[0] length] : 0;
        
        while (1) {
            BOOL moved = NO;
            NSMutableArray<NSMutableString*> *next = [NSMutableArray array];
            for (NSMutableString *row in grid) [next addObject:[[NSMutableString alloc] initWithString:row]];
            
            // East
            for (int r = 0; r < rows; r++) {
                for (int c = 0; c < cols; c++) {
                    if ([grid[r] characterAtIndex:c] == '>') {
                        int nc = (c + 1) % cols;
                        if ([grid[r] characterAtIndex:nc] == '.') {
                            [next[r] replaceCharactersInRange:NSMakeRange(c, 1) withString:@"."];
                            [next[r] replaceCharactersInRange:NSMakeRange(nc, 1) withString:@">"];
                            moved = YES;
                        }
                    }
                }
            }
            grid = next;
            next = [NSMutableArray array];
            for (NSMutableString *row in grid) [next addObject:[[NSMutableString alloc] initWithString:row]];
            
            // South
            for (int c = 0; c < cols; c++) {
                for (int r = 0; r < rows; r++) {
                    if ([grid[r] characterAtIndex:c] == 'v') {
                        int nr = (r + 1) % rows;
                        if ([grid[nr] characterAtIndex:c] == '.') {
                            [next[r] replaceCharactersInRange:NSMakeRange(c, 1) withString:@"."];
                            [next[nr] replaceCharactersInRange:NSMakeRange(c, 1) withString:@"v"];
                            moved = YES;
                        }
                    }
                }
            }
            grid = next;
            steps++;
            if (!moved) break;
        }
        printf("%d\n", steps);
    }
    return 0;
}