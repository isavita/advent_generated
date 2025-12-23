
#import <Foundation/Foundation.h>
#import <ctype.h>

int shortestPath(NSArray<NSMutableString *> *grid,
                 int height, int width,
                 int sx, int sy,
                 int keyMap[26], int keyCount) {
    int target = (1 << keyCount) - 1;
    int dirs[4][2] = {{0,-1},{-1,0},{0,1},{1,0}};
    NSMutableArray *queue = [NSMutableArray array];
    NSMutableSet *visited = [NSMutableSet set];
    NSString *startKey = [NSString stringWithFormat:@"%d,%d,%d", sx, sy, 0];
    [queue addObject:startKey];
    [visited addObject:startKey];
    int steps = 0;
    int front = 0;

    while (front < [queue count]) {
        int levelSize = [queue count] - front;
        for (int i = 0; i < levelSize; i++) {
            NSString *stateStr = queue[front++];
            int x, y, keys;
            sscanf([stateStr UTF8String], "%d,%d,%d", &x, &y, &keys);
            if (keys == target) return steps;

            for (int d = 0; d < 4; d++) {
                int nx = x + dirs[d][0];
                int ny = y + dirs[d][1];
                if (nx < 0 || nx >= width || ny < 0 || ny >= height) continue;
                unichar cell = [grid[ny] characterAtIndex:nx];
                if (cell == '#') continue;

                int nkeys = keys;
                if (cell >= 'a' && cell <= 'z') {
                    int idx = cell - 'a';
                    if (keyMap[idx] != -1) nkeys |= 1 << keyMap[idx];
                } else if (cell >= 'A' && cell <= 'Z') {
                    int idx = (cell | 0x20) - 'a';
                    if (keyMap[idx] == -1 || !(keys & (1 << keyMap[idx]))) continue;
                }

                NSString *nextState = [NSString stringWithFormat:@"%d,%d,%d", nx, ny, nkeys];
                if (![visited containsObject:nextState]) {
                    [visited addObject:nextState];
                    [queue addObject:nextState];
                }
            }
        }
        steps++;
    }
    return -1;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                       encoding:NSUTF8StringEncoding
                                                          error:&err];
        if (!content) return 1;
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSMutableString *> *grid = [NSMutableArray array];
        int width = -1, height = 0;
        int sx = -1, sy = -1;
        int keyMap[26];
        for (int i = 0; i < 26; i++) keyMap[i] = -1;
        int keyCount = 0;
        for (NSString *raw in lines) {
            NSString *line = [raw stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            if ([line length] == 0) continue;
            if (width == -1) width = [line length];
            else if ((int)[line length] != width) return 1;
            NSMutableString *row = [line mutableCopy];
            for (int x = 0; x < width; x++) {
                unichar ch = [row characterAtIndex:x];
                if (ch == '@') {
                    sx = x; sy = height;
                    [row replaceCharactersInRange:NSMakeRange(x,1) withString:@"."];
                } else if (ch >= 'a' && ch <= 'z') {
                    int idx = ch - 'a';
                    if (keyMap[idx] == -1) keyMap[idx] = keyCount++;
                }
            }
            [grid addObject:row];
            height++;
        }
        if (sx == -1) return 1;
        if (keyCount == 0) { printf("0\n"); return 0; }
        int res = shortestPath(grid, height, width, sx, sy, keyMap, keyCount);
        printf("%d\n", res);
    }
    return 0;
}
