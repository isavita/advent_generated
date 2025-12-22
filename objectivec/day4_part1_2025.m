#import <Foundation/Foundation.h>

#define MAX 1024

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                       encoding:NSUTF8StringEncoding
                                                          error:nil];
        if (!content) return 1;
        
        NSArray *lines = [content componentsSeparatedByCharactersInSet:
                          [NSCharacterSet newlineCharacterSet]];
        
        char grid[MAX][MAX];
        int rows = 0, cols = 0;
        
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            strcpy(grid[rows], [line UTF8String]);
            if (!cols) cols = (int)line.length;
            rows++;
        }
        
        int dx[8] = {-1,-1,-1, 0, 0, 1, 1, 1};
        int dy[8] = {-1, 0, 1,-1, 1,-1, 0, 1};
        
        int acc = 0;
        for (int y = 0; y < rows; y++) {
            for (int x = 0; x < cols; x++) {
                if (grid[y][x] != '@') continue;
                int cnt = 0;
                for (int d = 0; d < 8; d++) {
                    int nx = x + dx[d], ny = y + dy[d];
                    if (nx >= 0 && nx < cols && ny >= 0 && ny < rows && grid[ny][nx] == '@') cnt++;
                }
                if (cnt < 4) acc++;
            }
        }
        printf("%d\n", acc);
    }
    return 0;
}