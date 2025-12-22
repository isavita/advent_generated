
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
        NSString *text = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [text componentsSeparatedByString:@"\n"];
        
        int width = 0, height = 0;
        char grid[1024][1024];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            strcpy(grid[height], [line UTF8String]);
            if (!width) width = strlen(grid[height]);
            height++;
        }
        
        int sx = 0, sy = 0;
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                if (grid[y][x] == 'S') { sx = x; sy = y; goto found; }
    found:;
        
        int splits = 0;
        char active[width+1];
        memset(active, 0, sizeof(active));
        active[sx] = 1;
        
        for (int y = sy; y < height; y++) {
            char next[width+1];
            memset(next, 0, sizeof(next));
            int any = 0;
            for (int x = 0; x < width; x++) {
                if (!active[x]) continue;
                if (grid[y][x] == '^') {
                    splits++;
                    if (x - 1 >= 0) { next[x - 1] = 1; any = 1; }
                    if (x + 1 < width) { next[x + 1] = 1; any = 1; }
                } else {
                    next[x] = 1;
                    any = 1;
                }
            }
            memcpy(active, next, sizeof(active));
            if (!any) break;
        }
        
        printf("%d\n", splits);
    }
    return 0;
}
