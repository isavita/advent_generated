
#import <Foundation/Foundation.h>

static const int DR[4] = {1,-1, 0, 0};
static const int DC[4] = {0, 0, 1,-1};

int nr, nc;
int *grid;
long long *dp;

static inline int idx(int r, int c) { return r*nc + c; }

long long dfs(int r, int c) {
    int i = idx(r,c);
    if (dp[i] != -1) return dp[i];
    int h = grid[i];
    if (h == 9) return dp[i] = 1;
    long long sum = 0;
    for (int d = 0; d < 4; d++) {
        int nr2 = r + DR[d], nc2 = c + DC[d];
        if ((unsigned)nr2 < nr && (unsigned)nc2 < nc &&
            grid[idx(nr2,nc2)] == h + 1)
            sum += dfs(nr2, nc2);
    }
    return dp[i] = sum;
}

int main(int argc, char **argv) {
    @autoreleasepool {
        NSString *file = [NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        NSArray *lines = [file componentsSeparatedByCharactersInSet:
                         [NSCharacterSet newlineCharacterSet]];
        nr = (int)lines.count;
        for (NSString *l in lines) {
            nc = (int)l.length;
            if (nc) break;
        }
        grid = malloc(nr*nc*sizeof(int));
        dp   = malloc(nr*nc*sizeof(long long));
        for (int i = 0; i < nr*nc; i++) dp[i] = -1;
        int r = 0;
        for (NSString *l in lines) {
            for (int c = 0; c < nc; c++)
                grid[idx(r,c)] = [l characterAtIndex:c] - '0';
            r++;
        }
        long long total = 0;
        for (int i = 0; i < nr*nc; i++)
            if (grid[i] == 0) total += dfs(i/nc, i%nc);
        printf("%lld\n", total);
        free(grid); free(dp);
    }
    return 0;
}
