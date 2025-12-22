
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdbool.h>

typedef struct {int dx, dy;} Direction;

static const Direction dirs[] = {
    {0,1},{1,0},{1,1},{-1,1},{0,-1},{-1,0},{-1,-1},{1,-1}
};

static bool checkWord(const char **g, int rows, int cols, const char *w,
                      int x, int y, Direction d) {
    int len = (int)strlen(w);
    for (int i = 0; i < len; ++i) {
        int nx = x + d.dx * i;
        int ny = y + d.dy * i;
        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols) return false;
        if (g[nx][ny] != w[i]) return false;
    }
    return true;
}

static int countOccurrences(const char **g, int rows, int cols, const char *w) {
    int cnt = 0;
    int nd = (int)(sizeof(dirs)/sizeof(dirs[0]));
    for (int i = 0; i < rows; ++i)
        for (int j = 0; j < cols; ++j)
            for (int k = 0; k < nd; ++k)
                if (checkWord(g, rows, cols, w, i, j, dirs[k]))
                    ++cnt;
    return cnt;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:path
                                                      encoding:NSUTF8StringEncoding
                                                         error:&err];
        if (!content) { perror("Error opening file"); return 1; }

        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:
                                      [NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSString *> *gridStr = [NSMutableArray array];
        for (NSString *ln in lines) if (ln.length) [gridStr addObject:ln];

        int rows = (int)gridStr.count;
        if (rows == 0) { printf("Empty grid\n"); return 0; }
        int cols = (int)[gridStr[0] length];

        const char **grid = malloc(rows * sizeof(char *));
        for (int i = 0; i < rows; ++i)
            grid[i] = strdup([gridStr[i] UTF8String]);

        int cnt = countOccurrences(grid, rows, cols, "XMAS");
        printf("XMAS appears %d times in the word search\n", cnt);

        for (int i = 0; i < rows; ++i) free((void *)grid[i]);
        free(grid);
    }
    return 0;
}
