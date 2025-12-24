#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        int h = (int)[lines count], w = (int)[lines[0] length];
        int grid[h][w];
        for (int y = 0; y < h; y++) {
            NSString *row = lines[y];
            for (int x = 0; x < w; x++)
                grid[y][x] = [row characterAtIndex:x] - '0';
        }
        int best = 0;
        int dx[] = {0,0,1,-1}, dy[] = {1,-1,0,0};
        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++) {
                int score = 1;
                for (int d = 0; d < 4; d++) {
                    int nx = x, ny = y, dist = 0;
                    while (1) {
                        nx += dx[d]; ny += dy[d];
                        if (nx < 0 || nx >= w || ny < 0 || ny >= h) break;
                        dist++;
                        if (grid[ny][nx] >= grid[y][x]) break;
                    }
                    score *= dist;
                }
                if (score > best) best = score;
            }
        }
        printf("%d\n", best);
    }
    return 0;
}