
#import <Foundation/Foundation.h>

typedef struct { int r, c; } Pos;
typedef struct { Pos p; int h; } Node;

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if (!content) return 1;
        NSArray<NSString *> *rawLines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSString *> *lines = [NSMutableArray array];
        for (NSString *l in rawLines) {
            if (l.length) [lines addObject:l];
        }
        int nr = (int)lines.count;
        int nc = (int)lines[0].length;
        int **grid = malloc(sizeof(int*) * nr);
        for (int i = 0; i < nr; i++) {
            grid[i] = malloc(sizeof(int) * nc);
            const char *cstr = lines[i].UTF8String;
            for (int j = 0; j < nc; j++) grid[i][j] = cstr[j] - '0';
        }
        Pos *trailheads = NULL;
        int thCount = 0;
        for (int r = 0; r < nr; r++) {
            for (int c = 0; c < nc; c++) {
                if (grid[r][c] == 0) {
                    trailheads = realloc(trailheads, sizeof(Pos) * (thCount + 1));
                    trailheads[thCount++] = (Pos){r, c};
                }
            }
        }
        Pos dirs[4] = {{1,0},{-1,0},{0,1},{0,-1}};
        long long sum = 0;
        for (int i = 0; i < thCount; i++) {
            Pos th = trailheads[i];
            char *reached = calloc(nr * nc, 1);
            Node *queue = malloc(sizeof(Node) * nr * nc);
            int head = 0, tail = 0;
            queue[tail++] = (Node){th, 0};
            char *visited = calloc(nr * nc * 10, 1);
            visited[(th.r * nc + th.c) * 10] = 1;
            while (head < tail) {
                Node cur = queue[head++];
                if (cur.h == 9) {
                    reached[cur.p.r * nc + cur.p.c] = 1;
                    continue;
                }
                for (int d = 0; d < 4; d++) {
                    int nr2 = cur.p.r + dirs[d].r;
                    int nc2 = cur.p.c + dirs[d].c;
                    if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue;
                    if (grid[nr2][nc2] == cur.h + 1) {
                        int idx = (nr2 * nc + nc2) * 10 + cur.h + 1;
                        if (!visited[idx]) {
                            visited[idx] = 1;
                            queue[tail++] = (Node){{nr2, nc2}, cur.h + 1};
                        }
                    }
                }
            }
            free(queue);
            free(visited);
            int cnt = 0;
            for (int j = 0; j < nr * nc; j++) if (reached[j]) cnt++;
            sum += cnt;
            free(reached);
        }
        free(trailheads);
        for (int i = 0; i < nr; i++) free(grid[i]);
        free(grid);
        printf("%lld\n", sum);
    }
    return 0;
}
