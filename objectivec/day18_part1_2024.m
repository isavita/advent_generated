
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        const int GRID_SIZE = 71;
        BOOL corrupted[GRID_SIZE][GRID_SIZE] = {0};
        BOOL visited[GRID_SIZE][GRID_SIZE] = {0};

        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        if (!content) return 1;
        NSArray *lines = [content componentsSeparatedByString:@"\n"];

        for (int i = 0; i < 1024 && i < lines.count; i++) {
            NSString *line = lines[i];
            NSArray *parts = [line componentsSeparatedByString:@","];
            if (parts.count != 2) continue;
            int x = [parts[0] intValue];
            int y = [parts[1] intValue];
            if (x >= 0 && x < GRID_SIZE && y >= 0 && y < GRID_SIZE)
                corrupted[x][y] = YES;
        }

        typedef struct { int x, y, steps; } Node;
        NSMutableArray *q = [NSMutableArray array];
        Node start = {0, 0, 0};
        [q addObject:[NSValue valueWithBytes:&start objCType:@encode(Node)]];
        visited[0][0] = YES;

        int dr[] = {0, 0, 1, -1};
        int dc[] = {1, -1, 0, 0};

        while (q.count) {
            Node cur;
            [[q objectAtIndex:0] getValue:&cur];
            [q removeObjectAtIndex:0];
            if (cur.x == GRID_SIZE - 1 && cur.y == GRID_SIZE - 1) {
                printf("%d\n", cur.steps);
                return 0;
            }
            for (int i = 0; i < 4; i++) {
                int nx = cur.x + dr[i];
                int ny = cur.y + dc[i];
                if (nx >= 0 && nx < GRID_SIZE && ny >= 0 && ny < GRID_SIZE &&
                    !corrupted[nx][ny] && !visited[nx][ny]) {
                    visited[nx][ny] = YES;
                    Node next = {nx, ny, cur.steps + 1};
                    [q addObject:[NSValue valueWithBytes:&next objCType:@encode(Node)]];
                }
            }
        }
    }
    return 0;
}
