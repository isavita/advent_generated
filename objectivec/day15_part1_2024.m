#import <Foundation/Foundation.h>

static char **grid;
static int rows, cols;
static char *moves;
static int movesLen, movesCap;
static int robotR, robotC;

static int pushBoxes(int r, int c, int dr, int dc) {
    int nr = r + dr, nc = c + dc;
    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) return 0;
    if (grid[nr][nc] == '#') return 0;
    if (grid[nr][nc] == 'O' && !pushBoxes(nr, nc, dr, dc)) return 0;
    if (grid[nr][nc] != '.') return 0;
    grid[nr][nc] = 'O';
    grid[r][c] = '.';
    return 1;
}

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        if (!content) return 1;
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        BOOL readingMap = YES;
        for (NSString *lineObj in lines) {
            const char *line = lineObj.UTF8String;
            if (!line) continue;
            int len = (int)strlen(line);
            if (!len) { if (rows) readingMap = NO; continue; }
            if (readingMap && (strchr(line, '#') || strchr(line, '@') || strchr(line, 'O') || strchr(line, '.'))) {
                if (rows >= cols) cols = len;
                grid = realloc(grid, (rows + 1) * sizeof *grid);
                grid[rows] = malloc((cols + 1) * sizeof *grid[rows]);
                strcpy(grid[rows], line);
                char *at = strchr(grid[rows], '@');
                if (at) { robotR = rows; robotC = (int)(at - grid[rows]); }
                rows++;
            } else {
                readingMap = NO;
                int add = (int)strlen(line);
                if (movesLen + add + 1 > movesCap) {
                    movesCap = movesCap ? movesCap * 2 : 128;
                    while (movesCap < movesLen + add + 1) movesCap *= 2;
                    moves = realloc(moves, movesCap);
                }
                if (!movesLen) moves[0] = 0;
                strcat(moves, line);
                movesLen += add;
            }
        }
        for (int i = 0; i < movesLen; i++) {
            int dr = 0, dc = 0;
            switch (moves[i]) {
                case '^': dr = -1; break;
                case 'v': dr = 1; break;
                case '<': dc = -1; break;
                case '>': dc = 1; break;
                default: continue;
            }
            int nr = robotR + dr, nc = robotC + dc;
            if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
            if (grid[nr][nc] == '#') continue;
            if (grid[nr][nc] == 'O' && !pushBoxes(nr, nc, dr, dc)) continue;
            grid[robotR][robotC] = '.';
            grid[nr][nc] = '@';
            robotR = nr; robotC = nc;
        }
        long long total = 0;
        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
                if (grid[r][c] == 'O') total += r * 100LL + c;
        printf("%lld\n", total);
        for (int i = 0; i < rows; i++) free(grid[i]);
        free(grid); free(moves);
    }
    return 0;
}