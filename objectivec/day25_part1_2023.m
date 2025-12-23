
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdbool.h>

#define MAX_LINE_LEN 256
#define INITIAL_CAPACITY 16
#define VERTEX_NAME_LEN 8

typedef struct {
    char name[VERTEX_NAME_LEN];
    int id;
    bool occupied;
} VertexEntry;

static VertexEntry *vertex_map = NULL;
static int vertex_map_size = 0, vertex_map_capacity = 0, vertex_count = 0;

static unsigned int hash_function(const char *s) {
    unsigned int h = 5381;
    int c;
    while ((c = *s++)) h = ((h << 5) + h) + c;
    return h;
}
static void resize_vertex_map(void) {
    int old = vertex_map_capacity;
    VertexEntry *old_map = vertex_map;
    vertex_map_capacity = vertex_map_capacity ? vertex_map_capacity * 2 : INITIAL_CAPACITY;
    vertex_map = calloc(vertex_map_capacity, sizeof(VertexEntry));
    vertex_map_size = 0;
    for (int i = 0; i < old; ++i) if (old_map[i].occupied) {
        unsigned int h = hash_function(old_map[i].name) % vertex_map_capacity;
        while (vertex_map[h].occupied) h = (h + 1) % vertex_map_capacity;
        vertex_map[h] = old_map[i];
        vertex_map_size++;
    }
    free(old_map);
}
static int get_vertex_id(const char *n) {
    if (vertex_map_size * 2 >= vertex_map_capacity) resize_vertex_map();
    unsigned int h = hash_function(n) % vertex_map_capacity;
    while (vertex_map[h].occupied) {
        if (!strcmp(vertex_map[h].name, n)) return vertex_map[h].id;
        h = (h + 1) % vertex_map_capacity;
    }
    strncpy(vertex_map[h].name, n, VERTEX_NAME_LEN - 1);
    vertex_map[h].name[VERTEX_NAME_LEN - 1] = 0;
    vertex_map[h].id = vertex_count++;
    vertex_map[h].occupied = true;
    vertex_map_size++;
    return vertex_map[h].id;
}

typedef struct {
    int u, v;
    bool active;
    int id;
} Edge;
typedef struct {
    int neighbor_id;
    int edge_id;
} AdjNode;
typedef struct {
    AdjNode *nodes;
    int count, capacity;
} AdjList;

static Edge *edges = NULL;
static int edge_count = 0, edge_capacity = 0;
static AdjList *adj = NULL;
static int adj_capacity = 0;

static void ensure_adj_capacity(int n) {
    if (n <= adj_capacity) return;
    int newc = adj_capacity ? adj_capacity : INITIAL_CAPACITY;
    while (newc < n) newc <<= 1;
    adj = realloc(adj, newc * sizeof(AdjList));
    for (int i = adj_capacity; i < newc; ++i) adj[i].nodes = NULL, adj[i].count = adj[i].capacity = 0;
    adj_capacity = newc;
}
static void add_adj_node(int u, int v, int e) {
    ensure_adj_capacity(u + 1);
    if (adj[u].count >= adj[u].capacity) {
        adj[u].capacity = adj[u].capacity ? adj[u].capacity * 2 : 4;
        adj[u].nodes = realloc(adj[u].nodes, adj[u].capacity * sizeof(AdjNode));
    }
    adj[u].nodes[adj[u].count++] = (AdjNode){v, e};
}
static void add_edge(int u, int v) {
    if (edge_count >= edge_capacity) {
        edge_capacity = edge_capacity ? edge_capacity * 2 : INITIAL_CAPACITY;
        edges = realloc(edges, edge_capacity * sizeof(Edge));
    }
    edges[edge_count] = (Edge){u, v, true, edge_count};
    add_adj_node(u, v, edge_count);
    add_adj_node(v, u, edge_count);
    edge_count++;
}
typedef struct {
    bool found;
    int *parent_edge;
} BfsResult;
static BfsResult bfs(int s, int t) {
    BfsResult r = {false, NULL};
    if (s >= vertex_count || (t != -1 && t >= vertex_count)) return r;
    r.parent_edge = malloc(vertex_count * sizeof(int));
    bool *vis = calloc(vertex_count, 1);
    int *q = malloc(vertex_count * sizeof(int));
    for (int i = 0; i < vertex_count; ++i) r.parent_edge[i] = -1;
    int h = 0, tl = 0;
    q[tl++] = s; vis[s] = true;
    while (h < tl) {
        int cur = q[h++];
        if (t != -1 && cur == t) { r.found = true; break; }
        ensure_adj_capacity(cur + 1);
        for (int i = 0; i < adj[cur].count; ++i) {
            AdjNode an = adj[cur].nodes[i];
            if (!edges[an.edge_id].active) continue;
            int nb = an.neighbor_id;
            if (!vis[nb]) {
                vis[nb] = true;
                r.parent_edge[nb] = an.edge_id;
                q[tl++] = nb;
            }
        }
    }
    if (t == -1) r.found = true;
    free(vis); free(q);
    if (t != -1 && !r.found) { free(r.parent_edge); r.parent_edge = NULL; }
    return r;
}
static int reconstruct(int s, int t, int *par, int *buf, int max) {
    int cnt = 0, cur = t;
    while (cur != s && par[cur] != -1 && cnt < max) {
        int e = par[cur];
        buf[cnt++] = e;
        cur = (edges[e].u == cur) ? edges[e].v : edges[e].u;
    }
    for (int i = 0; i < cnt/2; ++i) {
        int tmp = buf[i]; buf[i] = buf[cnt-1-i]; buf[cnt-1-i] = tmp;
    }
    return cnt;
}
static long long solve(void) {
    int min_cut = 3, src = 0, compSize = -1;
    int *pathBuf = malloc(vertex_count * sizeof(int));
    for (int target = 1; target < vertex_count; ++target) {
        for (int i = 0; i < edge_count; ++i) edges[i].active = true;
        int found = 0;
        for (int k = 0; k < min_cut; ++k) {
            BfsResult br = bfs(src, target);
            if (!br.found) { free(br.parent_edge); break; }
            int plen = reconstruct(src, target, br.parent_edge, pathBuf, vertex_count);
            for (int i = 0; i < plen; ++i) edges[pathBuf[i]].active = false;
            free(br.parent_edge);
            ++found;
        }
        if (found == min_cut) {
            BfsResult chk = bfs(src, target);
            bool dis = !chk.found;
            free(chk.parent_edge);
            if (dis) {
                BfsResult comp = bfs(src, -1);
                int cnt = 0;
                for (int i = 0; i < vertex_count; ++i)
                    if (i == src || comp.parent_edge[i] != -1) ++cnt;
                compSize = cnt;
                free(comp.parent_edge);
                break;
            }
        }
    }
    free(pathBuf);
    if (compSize != -1) {
        long long a = compSize, b = vertex_count - compSize;
        return a * b;
    }
    return -1;
}
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *f = fopen("input.txt", "r");
        if (!f) { perror("open"); return 1; }
        char line[MAX_LINE_LEN];
        while (fgets(line, sizeof(line), f)) {
            line[strcspn(line, "\n")] = 0;
            char *v = strtok(line, ":");
            if (!v) continue;
            char *nbrs = strtok(NULL, "");
            if (!nbrs) continue;
            while (*nbrs == ' ') ++nbrs;
            int u = get_vertex_id(v);
            char *tok = strtok(nbrs, " ");
            while (tok) {
                int vId = get_vertex_id(tok);
                add_edge(u, vId);
                tok = strtok(NULL, " ");
            }
        }
        fclose(f);
        long long ans = solve();
        printf("%lld\n", ans);
        free(edges);
        if (adj) {
            for (int i = 0; i < adj_capacity; ++i) free(adj[i].nodes);
            free(adj);
        }
        free(vertex_map);
    }
    return 0;
}
