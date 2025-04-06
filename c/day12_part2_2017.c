
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_NODES 4096
#define INITIAL_CAPACITY 8

typedef struct {
    int* neighbors;
    int degree;
    int capacity;
} NodeAdj;

NodeAdj adj[MAX_NODES];
char visited[MAX_NODES] = {0};
int max_node_id = -1;

void add_neighbor(int u, int v) {
    if (u < 0 || u >= MAX_NODES || v < 0 || v >= MAX_NODES) return;

    if (adj[u].degree >= adj[u].capacity) {
        if (adj[u].capacity == 0) {
            adj[u].capacity = INITIAL_CAPACITY;
            adj[u].neighbors = (int*)malloc(adj[u].capacity * sizeof(int));
        } else {
            adj[u].capacity *= 2;
            adj[u].neighbors = (int*)realloc(adj[u].neighbors, adj[u].capacity * sizeof(int));
        }
        if (!adj[u].neighbors) {
             fprintf(stderr, "Memory allocation failed\n");
             exit(1);
        }
    }
    adj[u].neighbors[adj[u].degree++] = v;

    if (u > max_node_id) max_node_id = u;
    if (v > max_node_id) max_node_id = v;
}

void dfs(int u) {
    visited[u] = 1;
    for (int i = 0; i < adj[u].degree; ++i) {
        int v = adj[u].neighbors[i];
        if (!visited[v]) {
            dfs(v);
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        return 1;
    }

    char line[1024];
    while (fgets(line, sizeof(line), file)) {
        char *ptr = line;
        int u = strtol(ptr, &ptr, 10);

        while (*ptr && !isdigit(*ptr)) ptr++;

        while (*ptr) {
            if (isdigit(*ptr)) {
                int v = strtol(ptr, &ptr, 10);
                add_neighbor(u, v);
                add_neighbor(v, u);
            } else {
                ptr++;
            }
        }
    }
    fclose(file);

    int groups = 0;
    for (int i = 0; i <= max_node_id; ++i) {
         // Only check nodes that actually exist (have neighbors allocated)
        if (adj[i].capacity > 0 && !visited[i]) {
            dfs(i);
            groups++;
        }
    }

    printf("%d\n", groups);

    for (int i = 0; i <= max_node_id; ++i) {
        if (adj[i].neighbors) {
            free(adj[i].neighbors);
        }
    }

    return 0;
}
