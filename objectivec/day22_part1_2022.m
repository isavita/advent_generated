
#import <Foundation/Foundation.h>

typedef struct {int min;int max;} Boundary;
typedef struct {int x;int y;} Position;

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        if (!data) return 1;
        NSArray *parts = [data componentsSeparatedByString:@"\n\n"];
        if (parts.count < 2) return 1;
        NSArray *rawLines = [[parts[0] componentsSeparatedByString:@"\n"]
                             filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"self != ''"]];
        NSString *pathStr = [[parts[1] componentsSeparatedByString:@"\n"] firstObject];
        int rows = (int)rawLines.count;
        int cols = 0;
        for (NSString *ln in rawLines) {
            if ((int)ln.length > cols) cols = (int)ln.length;
        }
        char **grid = malloc(rows * sizeof(char *));
        for (int r = 0; r < rows; ++r) {
            grid[r] = malloc(cols + 1);
            const char *cstr = [(NSString *)rawLines[r] UTF8String];
            int len = (int)strlen(cstr);
            memcpy(grid[r], cstr, len);
            memset(grid[r] + len, ' ', cols - len);
            grid[r][cols] = '\0';
        }
        Boundary *rowB = calloc(rows, sizeof(Boundary));
        Boundary *colB = calloc(cols, sizeof(Boundary));
        for (int y = 0; y < rows; ++y) {
            rowB[y].min = 0;
            for (int x = 0; x < cols; ++x) if (grid[y][x] != ' ') {
                if (rowB[y].min == 0) rowB[y].min = x + 1;
                rowB[y].max = x + 1;
            }
        }
        for (int x = 0; x < cols; ++x) {
            colB[x].min = 0;
            for (int y = 0; y < rows; ++y) if (grid[y][x] != ' ') {
                if (colB[x].min == 0) colB[x].min = y + 1;
                colB[x].max = y + 1;
            }
        }
        Position pos = {0,1};
        for (int x = 0; x < cols; ++x) if (grid[0][x] == '.') { pos.x = x + 1; break; }
        int facing = 0;
        const char *p = [pathStr UTF8String];
        int dx[4] = {1,0,-1,0}, dy[4] = {0,1,0,-1};
        while (*p) {
            if (isdigit(*p)) {
                int steps = 0;
                while (isdigit(*p)) steps = steps*10 + (*p++ - '0');
                for (int i=0;i<steps;++i){
                    Position nxt = pos;
                    nxt.x += dx[facing];
                    nxt.y += dy[facing];
                    int yIdx = pos.y-1, xIdx = pos.x-1;
                    if (facing==0 && nxt.x>rowB[yIdx].max) nxt.x=rowB[yIdx].min;
                    if (facing==2 && nxt.x<rowB[yIdx].min) nxt.x=rowB[yIdx].max;
                    if (facing==1 && nxt.y>colB[xIdx].max) nxt.y=colB[xIdx].min;
                    if (facing==3 && nxt.y<colB[xIdx].min) nxt.y=colB[xIdx].max;
                    char tile = grid[nxt.y-1][nxt.x-1];
                    if (tile=='#') break;
                    if (tile=='.') pos=nxt;
                }
            } else {
                facing = (*p=='R') ? (facing+1)%4 : (facing+3)%4;
                ++p;
            }
        }
        long long password = 1000LL*pos.y + 4LL*pos.x + facing;
        printf("%lld\n", password);
        for (int i=0;i<rows;++i) free(grid[i]);
        free(grid); free(rowB); free(colB);
    }
    return 0;
}
