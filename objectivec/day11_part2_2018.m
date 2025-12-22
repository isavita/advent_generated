
#import <Foundation/Foundation.h>

#define N 300

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:nil];
        int serial = txt.intValue;

        int grid[N][N];
        for (int y = 0; y < N; ++y)
            for (int x = 0; x < N; ++x) {
                int id = x + 11;
                int p = id * (y + 1) + serial;
                grid[y][x] = ((p * id / 100) % 10) - 5;
            }

        int sum[N+1][N+1];
        memset(sum, 0, sizeof(sum));
        for (int y = 1; y <= N; ++y)
            for (int x = 1; x <= N; ++x)
                sum[y][x] = grid[y-1][x-1] + sum[y-1][x] + sum[y][x-1] - sum[y-1][x-1];

        int best = INT_MIN, bx = 0, by = 0, bs = 0;
        for (int s = 1; s <= N; ++s) {
            int area = s * s;
            if (area > 0 && best > area * 4) break;          // quick prune
            for (int y = 1; y <= N - s + 1; ++y)
                for (int x = 1; x <= N - s + 1; ++x) {
                    int p = sum[y+s-1][x+s-1] - sum[y+s-1][x-1] - sum[y-1][x+s-1] + sum[y-1][x-1];
                    if (p > best) { best = p; bx = x; by = y; bs = s; }
                }
        }
        printf("%d,%d,%d\n", bx, by, bs);
    }
    return 0;
}
