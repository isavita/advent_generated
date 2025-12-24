#import <Foundation/Foundation.h>

@interface Region : NSObject
@property (nonatomic) int area;
@property (nonatomic) int perimeter;
@end

@implementation Region
@end

static void walk(char **grid, int R, int C, int r, int c,
                 BOOL **seen, Region *reg) {
    char crop = grid[r][c];
    NSMutableArray *q = [NSMutableArray array];
    [q addObject:@{@"r":@(r), @"c":@(c)}];
    seen[r][c] = YES;

    while (q.count) {
        NSDictionary *p = q[0]; [q removeObjectAtIndex:0];
        int rr = [p[@"r"] intValue], cc = [p[@"c"] intValue];
        reg.area++;

        int dr[] = {-1,1,0,0}, dc[] = {0,0,-1,1};
        for (int i = 0; i < 4; ++i) {
            int nr = rr + dr[i], nc = cc + dc[i];
            if (nr < 0 || nr >= R || nc < 0 || nc >= C) {
                reg.perimeter++; continue;
            }
            if (grid[nr][nc] != crop) { reg.perimeter++; continue; }
            if (seen[nr][nc]) continue;
            seen[nr][nc] = YES;
            [q addObject:@{@"r":@(nr), @"c":@(nc)}];
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray *lines = [txt componentsSeparatedByString:@"\n"];
        int R = (int)lines.count;
        char **grid = calloc(R, sizeof(char *));
        for (int i = 0; i < R; ++i) {
            NSString *ln = lines[i];
            if ([ln length] == 0) continue;
            grid[i] = calloc([ln length] + 1, 1);
            strcpy(grid[i], [ln UTF8String]);
        }
        int C = (int)strlen(grid[0]);

        BOOL **seen = calloc(R, sizeof(BOOL *));
        for (int i = 0; i < R; ++i) seen[i] = calloc(C, sizeof(BOOL));

        int total = 0;
        for (int r = 0; r < R; ++r)
            for (int c = 0; c < C; ++c)
                if (!seen[r][c]) {
                    Region *reg = [[Region alloc] init];
                    walk(grid, R, C, r, c, seen, reg);
                    total += reg.area * reg.perimeter;
                }
        printf("%d\n", total);

        for (int i = 0; i < R; ++i) { free(grid[i]); free(seen[i]); }
        free(grid); free(seen);
    }
    return 0;
}