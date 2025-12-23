
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct { int x, y; } Coord;
typedef struct { int width, height; char **data; } Grid;
typedef struct { Coord start, end; int weight; } Edge;
typedef struct { int vertices_count; Coord *vertices; int edges_count; Edge *edges; int **adj; } Graph;

static const Coord North = {0, -1}, South = {0, 1}, West = {-1, 0}, East = {1, 0};
static const char Empty = '.', Wall = '#';

static int max(int a, int b) { return a > b ? a : b; }
static int isInBounds(Grid *g, Coord c) { return c.x >= 0 && c.x < g->width && c.y >= 0 && c.y < g->height; }
static int isValidNeighbor(Grid *g, Coord c) { return isInBounds(g, c) && g->data[c.y][c.x] != Wall; }
static Coord add(Coord a, Coord b) { return (Coord){a.x + b.x, a.y + b.y}; }

static Coord *neighbors4(Grid *g, Coord c, int *cnt) {
    Coord dirs[] = {North, South, West, East};
    Coord *out = malloc(4 * sizeof(Coord));
    *cnt = 0;
    for (int i = 0; i < 4; ++i) {
        Coord n = add(c, dirs[i]);
        if (isValidNeighbor(g, n)) out[(*cnt)++] = n;
    }
    return out;
}
static int vertexInGraph(Graph *g, Coord c) {
    for (int i = 0; i < g->vertices_count; ++i)
        if (g->vertices[i].x == c.x && g->vertices[i].y == c.y) return 1;
    return 0;
}
static int vertexIdx(Graph *g, Coord c) {
    for (int i = 0; i < g->vertices_count; ++i)
        if (g->vertices[i].x == c.x && g->vertices[i].y == c.y) return i;
    return -1;
}
static Grid parseInput(char **in, int h) {
    Grid g; g.height = h; g.width = strlen(in[0]);
    g.data = malloc(g.height * sizeof(char*));
    for (int y = 0; y < g.height; ++y) {
        g.data[y] = malloc(g.width);
        memcpy(g.data[y], in[y], g.width);
    }
    return g;
}
static Graph getGraph(Grid *g, Coord s, Coord e) {
    Graph gr; gr.vertices_count = 0; gr.vertices = malloc(sizeof(Coord));
    gr.edges_count = 0; gr.edges = malloc(sizeof(Edge));
    gr.vertices[gr.vertices_count++] = s;
    gr.vertices = realloc(gr.vertices, (gr.vertices_count + 1) * sizeof(Coord));
    gr.vertices[gr.vertices_count++] = e;
    gr.vertices = realloc(gr.vertices, (gr.vertices_count + 1) * sizeof(Coord));

    for (int y = 0; y < g->height; ++y)
        for (int x = 0; x < g->width; ++x)
            if (g->data[y][x] == Empty) {
                int nb; neighbors4(g, (Coord){x,y}, &nb);
                if (nb > 2 && !vertexInGraph(&gr, (Coord){x,y}))
                    gr.vertices[gr.vertices_count++] = (Coord){x,y},
                    gr.vertices = realloc(gr.vertices, (gr.vertices_count + 1) * sizeof(Coord));
            }

    gr.adj = malloc(gr.vertices_count * sizeof(int*));
    for (int i = 0; i < gr.vertices_count; ++i) {
        gr.adj[i] = calloc(gr.vertices_count, sizeof(int));
    }

    int size = g->width * g->height;
    for (int i = 0; i < gr.vertices_count; ++i) {
        Coord src = gr.vertices[i];
        Coord *queue = malloc(size * sizeof(Coord));
        int front = 0, rear = 0;
        queue[rear++] = src;
        int *dist = malloc(size * sizeof(int));
        memset(dist, -1, size * sizeof(int));
        dist[src.y * g->width + src.x] = 0;
        char **vis = malloc(g->height * sizeof(char*));
        for (int y = 0; y < g->height; ++y) {
            vis[y] = calloc(g->width, 1);
        }
        vis[src.y][src.x] = 1;

        while (front < rear) {
            Coord cur = queue[front++];
            if (vertexInGraph(&gr, cur) && !(cur.x == src.x && cur.y == src.y)) {
                int u = vertexIdx(&gr, src), v = vertexIdx(&gr, cur);
                gr.adj[u][v] = dist[cur.y * g->width + cur.x];
            } else {
                int nb; Coord *ne = neighbors4(g, cur, &nb);
                for (int k = 0; k < nb; ++k) {
                    Coord nxt = ne[k];
                    if (!vis[nxt.y][nxt.x]) {
                        vis[nxt.y][nxt.x] = 1;
                        queue[rear++] = nxt;
                        dist[nxt.y * g->width + nxt.x] = dist[cur.y * g->width + cur.x] + 1;
                    }
                }
                free(ne);
            }
        }
        for (int y = 0; y < g->height; ++y) free(vis[y]);
        free(vis); free(queue); free(dist);
    }
    return gr;
}
static int dfs(Graph *g, int u, int target, char *seen) {
    if (u == target) return 0;
    seen[u] = 1;
    int best = -1;
    for (int v = 0; v < g->vertices_count; ++v)
        if (g->adj[u][v] > 0 && !seen[v]) {
            int d = dfs(g, v, target, seen);
            if (d != -1) best = max(best, d + g->adj[u][v]);
        }
    seen[u] = 0;
    return best;
}
static int solve(char **in, int h) {
    Grid g = parseInput(in, h);
    Coord s = {1,0}, e = {g.width-2, g.height-1};
    Graph gr = getGraph(&g, s, e);
    char *seen = calloc(gr.vertices_count, 1);
    int ans = dfs(&gr, 0, 1, seen);
    free(seen);
    for (int i = 0; i < g.height; ++i) free(g.data[i]); free(g.data);
    for (int i = 0; i < gr.vertices_count; ++i) free(gr.adj[i]); free(gr.adj);
    free(gr.vertices); free(gr.edges);
    return ans;
}
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray *filtered = [NSMutableArray array];
        for (NSString *ln in lines) if (ln.length) [filtered addObject:ln];
        int cnt = (int)filtered.count;
        char **cLines = malloc(cnt * sizeof(char*));
        for (int i = 0; i < cnt; ++i) {
            const char *utf = [[filtered objectAtIndex:i] UTF8String];
            cLines[i] = malloc(strlen(utf) + 1);
            strcpy(cLines[i], utf);
        }
        int result = solve(cLines, cnt);
        printf("%d\n", result);
        for (int i = 0; i < cnt; ++i) free(cLines[i]);
        free(cLines);
    }
    return 0;
}
