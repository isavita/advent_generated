
#import <Foundation/Foundation.h>

static int maxSteps = 0;

void dfs(char **grid, int rows, int cols, int row, int col, int steps, bool **visited) {
    if (row < 0 || row >= rows || col < 0 || col >= cols || grid[row][col] == '#' || visited[row][col]) return;
    if (row == rows - 1) {
        if (steps > maxSteps) maxSteps = steps;
        return;
    }
    visited[row][col] = true;
    char c = grid[row][col];
    if (c == '.') {
        dfs(grid, rows, cols, row + 1, col, steps + 1, visited);
        dfs(grid, rows, cols, row - 1, col, steps + 1, visited);
        dfs(grid, rows, cols, row, col + 1, steps + 1, visited);
        dfs(grid, rows, cols, row, col - 1, steps + 1, visited);
    } else if (c == 'v') dfs(grid, rows, cols, row + 1, col, steps + 1, visited);
    else if (c == '^') dfs(grid, rows, cols, row - 1, col, steps + 1, visited);
    else if (c == '>') dfs(grid, rows, cols, row, col + 1, steps + 1, visited);
    else if (c == '<') dfs(grid, rows, cols, row, col - 1, steps + 1, visited);
    visited[row][col] = false;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        int rows = (int)[lines count];
        int cols = (int)[lines[0] length];
        char **grid = malloc(rows * sizeof(char *));
        for (int i = 0; i < rows; i++) {
            grid[i] = malloc(cols + 1);
            strcpy(grid[i], [lines[i] UTF8String]);
        }
        int startX = 0;
        while (grid[0][startX] != '.') startX++;
        bool **visited = malloc(rows * sizeof(bool *));
        for (int i = 0; i < rows; i++) {
            visited[i] = calloc(cols, sizeof(bool));
        }
        dfs(grid, rows, cols, 0, startX, 0, visited);
        printf("%d\n", maxSteps);
        for (int i = 0; i < rows; i++) {
            free(grid[i]);
            free(visited[i]);
        }
        free(grid);
        free(visited);
    }
    return 0;
}
