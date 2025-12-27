#import <Foundation/Foundation.h>

@interface State : NSObject
@property int row, col, time;
- (instancetype)initWithRow:(int)row col:(int)col time:(int)time;
- (NSUInteger)hash;
- (BOOL)isEqual:(id)object;
@end

@implementation State
- (instancetype)initWithRow:(int)row col:(int)col time:(int)time {
    if (self = [super init]) {
        self.row = row; self.col = col; self.time = time;
    }
    return self;
}
- (NSUInteger)hash { return self.row * 131071 + self.col * 127 + self.time; }
- (BOOL)isEqual:(State *)o {
    return self.row == o.row && self.col == o.col && self.time == o.time;
}
@end

static int solve(char **grid, int rows, int cols,
                 int sr, int sc, int er, int ec, int startTime) {
    NSMutableArray *q = [NSMutableArray array];
    State *init = [[State alloc] initWithRow:sr col:sc time:startTime];
    [q addObject:init];
    NSMutableSet *vis = [NSMutableSet set];
    [vis addObject:init];

    int dirs[5][2] = {{0,0},{0,1},{0,-1},{1,0},{-1,0}};
    while (q.count) {
        State *cur = q.firstObject;
        [q removeObjectAtIndex:0];
        if (cur.row == er && cur.col == ec) return cur.time;

        for (int d = 0; d < 5; d++) {
            int nr = cur.row + dirs[d][0];
            int nc = cur.col + dirs[d][1];
            int nt = cur.time + 1;
            if (nr<0||nr>=rows||nc<0||nc>=cols||grid[nr][nc]=='#') continue;

            BOOL hit = NO;
            int innerR = rows-2, innerC = cols-2;
            for (int i = 1; !hit && i < rows-1; i++) {
                for (int j = 1; j < cols-1; j++) {
                    char c = grid[i][j];
                    if (c=='.') continue;
                    int br=i, bc=j;
                    switch (c) {
                        case '^': br = 1 + ((i-1 - nt) % innerR + innerR) % innerR; break;
                        case 'v': br = 1 + ((i-1 + nt) % innerR + innerR) % innerR; break;
                        case '<': bc = 1 + ((j-1 - nt) % innerC + innerC) % innerC; break;
                        case '>': bc = 1 + ((j-1 + nt) % innerC + innerC) % innerC; break;
                    }
                    if (br==nr && bc==nc) { hit=YES; break; }
                }
            }
            if (!hit) {
                State *ns = [[State alloc] initWithRow:nr col:nc time:nt];
                if (![vis containsObject:ns]) {
                    [vis addObject:ns];
                    [q addObject:ns];
                }
            }
        }
    }
    return -1;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *data = [NSString stringWithContentsOfFile:path
                                                 encoding:NSUTF8StringEncoding
                                                    error:nil];
        NSArray *lines = [data componentsSeparatedByString:@"\n"];
        int rows = (int)lines.count;
        int cols = (int)[lines[0] length];
        char **grid = malloc(rows * sizeof(char*));
        for (int i=0;i<rows;i++){
            grid[i] = malloc(cols+1);
            strcpy(grid[i], [lines[i] UTF8String]);
        }
        int sr=0, sc=1;
        int er=rows-1, ec=cols-2;
        int t1 = solve(grid,rows,cols,sr,sc,er,ec,0);
        int t2 = solve(grid,rows,cols,er,ec,sr,sc,t1);
        int t3 = solve(grid,rows,cols,sr,sc,er,ec,t2);
        printf("%d\n%d\n",t1,t3);
        for(int i=0;i<rows;i++) free(grid[i]);
        free(grid);
    }
    return 0;
}