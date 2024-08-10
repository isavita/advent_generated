#import <Foundation/Foundation.h>

#define INF 999999

typedef struct {
    int x;
    int y;
} Point;

int minDistance(int dist[], BOOL sptSet[], int V) {
    int min = INF, minIndex;
    for (int v = 0; v < V; v++) {
        if (sptSet[v] == NO && dist[v] <= min) {
            min = dist[v];
            minIndex = v;
        }
    }
    return minIndex;
}

int dijkstra(int **graph, int V, Point start, Point end) {
    int *dist = malloc(V * sizeof(int));
    BOOL *sptSet = malloc(V * sizeof(BOOL));

    for (int i = 0; i < V; i++) {
        dist[i] = INF;
        sptSet[i] = NO;
    }

    dist[start.y * V + start.x] = 0;

    for (int count = 0; count < V - 1; count++) {
        int u = minDistance(dist, sptSet, V);
        sptSet[u] = YES;

        int ux = u % V;
        int uy = u / V;

        for (int v = 0; v < V; v++) {
            int vx = v % V;
            int vy = v / V;

            if (!sptSet[v] && graph[uy][ux] != -1 &&
                ((ux == vx && abs(uy - vy) == 1) || (uy == vy && abs(ux - vx) == 1))) {
                int alt = dist[u] + graph[vy][vx];
                if (alt < dist[v]) {
                    dist[v] = alt;
                }
            }
        }
    }

    int result = dist[end.y * V + end.x];
    free(dist);
    free(sptSet);
    return result;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];

        int n = (int)[lines count];
        int **grid = malloc(n * sizeof(int *));
        for (int i = 0; i < n; i++) {
            grid[i] = malloc(n * sizeof(int));
            NSString *line = lines[i];
            for (int j = 0; j < n; j++) {
                grid[i][j] = [[line substringWithRange:NSMakeRange(j, 1)] intValue];
            }
        }

        Point start = {0, 0};
        Point end = {n - 1, n - 1};
        int result = dijkstra(grid, n * n, start, end);

        printf("The lowest total risk is: %d\n", result);

        for (int i = 0; i < n; i++) {
            free(grid[i]);
        }
        free(grid);
    }
    return 0;
}