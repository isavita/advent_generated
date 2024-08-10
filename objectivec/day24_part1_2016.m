#import <Foundation/Foundation.h>

// Define a structure to hold the coordinates
typedef struct {
    int x;
    int y;
} Coordinate;

// Function to read the map from the file
NSString *readMapFromFile(NSString *filePath) {
    NSError *error = nil;
    NSString *map = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"Error reading file: %@", error);
        return nil;
    }
    return map;
}

// Function to parse the map and find the locations of the numbers
NSMutableDictionary *parseMap(NSString *map) {
    NSMutableDictionary *locations = [NSMutableDictionary dictionary];
    NSArray *lines = [map componentsSeparatedByString:@"\n"];
    for (int y = 0; y < lines.count; y++) {
        NSString *line = lines[y];
        for (int x = 0; x < line.length; x++) {
            unichar charAtPoint = [line characterAtIndex:x];
            if (charAtPoint >= '0' && charAtPoint <= '9') {
                NSString *key = [NSString stringWithFormat:@"%c", charAtPoint];
                Coordinate coord = {x, y};
                NSValue *coordValue = [NSValue valueWithBytes:&coord objCType:@encode(Coordinate)];
                locations[key] = coordValue;
            }
        }
    }
    return locations;
}

// Function to find the shortest path between two points using BFS
int shortestPath(NSString *map, Coordinate start, Coordinate end) {
    NSArray *lines = [map componentsSeparatedByString:@"\n"];
    int width = (int)[lines[0] length];
    int height = (int)lines.count;

    int dx[] = {-1, 1, 0, 0};
    int dy[] = {0, 0, -1, 1};

    BOOL visited[height][width];
    memset(visited, 0, sizeof(visited));

    NSMutableArray *queue = [NSMutableArray array];
    [queue addObject:[NSValue valueWithBytes:&start objCType:@encode(Coordinate)]];
    visited[start.y][start.x] = YES;

    int steps = 0;
    while ([queue count] > 0) {
        int size = (int)[queue count];
        for (int i = 0; i < size; i++) {
            NSValue *currentValue = queue[i];
            Coordinate current;
            [currentValue getValue:&current];

            if (current.x == end.x && current.y == end.y) {
                return steps;
            }

            for (int j = 0; j < 4; j++) {
                int newX = current.x + dx[j];
                int newY = current.y + dy[j];
                if (newX >= 0 && newX < width && newY >= 0 && newY < height && !visited[newY][newX]) {
                    unichar charAtPoint = [lines[newY] characterAtIndex:newX];
                    if (charAtPoint == '.' || (charAtPoint >= '0' && charAtPoint <= '9')) {
                        Coordinate newCoord = {newX, newY};
                        [queue addObject:[NSValue valueWithBytes:&newCoord objCType:@encode(Coordinate)]];
                        visited[newY][newX] = YES;
                    }
                }
            }
        }
        [queue removeObjectsInRange:NSMakeRange(0, size)];
        steps++;
    }
    return INT_MAX;
}

// Function to find the shortest path to visit all points
int findShortestPathToVisitAllPoints(NSString *map, NSDictionary *locations) {
    NSArray *keys = [locations.allKeys sortedArrayUsingSelector:@selector(compare:)];
    int n = (int)keys.count;
    int dp[(1 << n)][n];
    memset(dp, 0x3f, sizeof(dp));
    dp[1][0] = 0;

    for (int mask = 0; mask < (1 << n); mask++) {
        for (int u = 0; u < n; u++) {
            if (mask & (1 << u)) {
                Coordinate startCoord;
                [[locations objectForKey:keys[u]] getValue:&startCoord];
                for (int v = 0; v < n; v++) {
                    if (!(mask & (1 << v))) {
                        Coordinate endCoord;
                        [[locations objectForKey:keys[v]] getValue:&endCoord];
                        int dist = shortestPath(map, startCoord, endCoord);
                        dp[mask | (1 << v)][v] = MIN(dp[mask | (1 << v)][v], dp[mask][u] + dist);
                    }
                }
            }
        }
    }

    int ans = INT_MAX;
    for (int i = 0; i < n; i++) {
        ans = MIN(ans, dp[(1 << n) - 1][i]);
    }
    return ans;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *map = readMapFromFile(filePath);
        if (!map) {
            return 1;
        }

        NSDictionary *locations = parseMap(map);
        int result = findShortestPathToVisitAllPoints(map, locations);
        NSLog(@"The fewest number of steps required is: %d", result);
    }
    return 0;
}