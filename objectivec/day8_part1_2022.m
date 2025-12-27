
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray<NSString *> *lines = [data componentsSeparatedByCharactersInSet:
                                      [NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSMutableArray<NSNumber *> *> *grid = [NSMutableArray array];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSMutableArray<NSNumber *> *row = [NSMutableArray array];
            for (NSUInteger i = 0; i < line.length; ++i) {
                unichar ch = [line characterAtIndex:i];
                [row addObject:@(ch - '0')];
            }
            [grid addObject:row];
        }
        NSInteger R = grid.count;
        if (R == 0) { printf("0\n"); return 0; }
        NSInteger C = grid[0].count;
        if (C == 0) { printf("0\n"); return 0; }

        NSMutableArray<NSMutableArray<NSNumber *> *> *vis =
            [NSMutableArray arrayWithCapacity:R];
        for (NSInteger r = 0; r < R; ++r) {
            NSMutableArray<NSNumber *> *row = [NSMutableArray arrayWithCapacity:C];
            for (NSInteger c = 0; c < C; ++c) [row addObject:@(0)];
            [vis addObject:row];
        }

        for (NSInteger r = 0; r < R; ++r) {
            NSInteger max = -1;
            for (NSInteger c = 0; c < C; ++c) {
                NSInteger v = grid[r][c].integerValue;
                if (v > max) { vis[r][c] = @1; max = v; }
            }
            max = -1;
            for (NSInteger c = C - 1; c >= 0; --c) {
                NSInteger v = grid[r][c].integerValue;
                if (v > max) { vis[r][c] = @1; max = v; }
                if (c == 0) break;
            }
        }

        for (NSInteger c = 0; c < C; ++c) {
            NSInteger max = -1;
            for (NSInteger r = 0; r < R; ++r) {
                NSInteger v = grid[r][c].integerValue;
                if (v > max) { vis[r][c] = @1; max = v; }
            }
            max = -1;
            for (NSInteger r = R - 1; r >= 0; --r) {
                NSInteger v = grid[r][c].integerValue;
                if (v > max) { vis[r][c] = @1; max = v; }
                if (r == 0) break;
            }
        }

        NSInteger count = 0;
        for (NSInteger r = 0; r < R; ++r)
            for (NSInteger c = 0; c < C; ++c)
                if (vis[r][c].boolValue) ++count;

        printf("%ld\n", (long)count);
    }
    return 0;
}
