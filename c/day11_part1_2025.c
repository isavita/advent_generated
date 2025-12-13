
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 256
#define MAX_NODES 1024
#define MAX_EDGES 64
#define HASH_SIZE 2048

typedef struct Node {
    char name[32];
    int edges[MAX_EDGES];
    int edge_cnt;
} Node;

static Node nodes[MAX_NODES];
static int node_cnt;
static int memo[MAX_NODES];

static int hash(const char *s) {
    unsigned h = 0;
    while (*s) h = h * 31 + *s++;
    return h % HASH_SIZE;
}

static int find_or_add(const char *name) {
    int h = hash(name);
    int idx = h;
    while (idx < MAX_NODES && nodes[idx].name[0]) {
        if (strcmp(nodes[idx].name, name) == 0) return idx;
        idx++;
    }
    if (idx >= MAX_NODES) {
        for (idx = 0; idx < h && nodes[idx].name[0]; idx++) {
            if (strcmp(nodes[idx].name, name) == 0) return idx;
        }
    }
    strcpy(nodes[idx].name, name);
    node_cnt++;
    return idx;
}

static int dfs(int u, int target) {
    if (u == target) return 1;
    if (memo[u] != -1) return memo[u];
    int total = 0;
    for (int i = 0; i < nodes[u].edge_cnt; i++) {
        total += dfs(nodes[u].edges[i], target);
    }
    return memo[u] = total;
}

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 0;
    char line[MAX_LINE];
    while (fgets(line, sizeof(line), f)) {
        char *src = strtok(line, ":");
        if (!src) continue;
        char *rest = strtok(NULL, "\n");
        if (!rest) continue;
        int u = find_or_add(src);
        char *tok = strtok(rest, " ");
        while (tok) {
            int v = find_or_add(tok);
            nodes[u].edges[nodes[u].edge_cnt++] = v;
            tok = strtok(NULL, " ");
        }
    }
    fclose(f);
    memset(memo, -1, sizeof(memo));
    int start = find_or_add("you");
    int end = find_or_add("out");
    printf("%d\n", dfs(start, end));
    return 0;
}
