
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char *name;
    int *nbr;
    int cnt, cap;
} Node;

Node *nodes = NULL;
int nnode = 0, capnode = 0;

int idx(const char *s) {
    for (int i = 0; i < nnode; ++i)
        if (strcmp(nodes[i].name, s) == 0) return i;
    if (nnode == capnode) {
        capnode = capnode ? capnode * 2 : 16;
        nodes = realloc(nodes, capnode * sizeof(Node));
    }
    nodes[nnode].name = strdup(s);
    nodes[nnode].nbr = NULL;
    nodes[nnode].cnt = nodes[nnode].cap = 0;
    return nnode++;
}

void add_edge(int u, int v) {
    Node *p = &nodes[u];
    if (p->cnt == p->cap) {
        p->cap = p->cap ? p->cap * 2 : 4;
        p->nbr = realloc(p->nbr, p->cap * sizeof(int));
    }
    p->nbr[p->cnt++] = v;
}

long long dfs(int cur, int tgt, long long *memo) {
    if (cur == tgt) return 1;
    if (memo[cur] != -1) return memo[cur];
    long long sum = 0;
    for (int i = 0; i < nodes[cur].cnt; ++i)
        sum += dfs(nodes[cur].nbr[i], tgt, memo);
    return memo[cur] = sum;
}

long long count_paths(int s, int t) {
    long long *memo = calloc(nnode, sizeof(long long));
    for (int i = 0; i < nnode; ++i) memo[i] = -1;
    long long res = dfs(s, t, memo);
    free(memo);
    return res;
}

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;
    char line[1024];
    while (fgets(line, sizeof line, f)) {
        char *p = strchr(line, '\n'); if (p) *p = 0;
        for (p = line; *p && (*p==' '||*p=='\t'); ++p);
        if (!*p) continue;
        char *colon = strchr(p, ':');
        if (!colon) continue;
        *colon = 0;
        char *src = p;
        char *dst = colon + 1;
        int u = idx(src);
        char *tok = strtok(dst, " \t");
        while (tok) {
            int v = idx(tok);
            add_edge(u, v);
            tok = strtok(NULL, " \t");
        }
    }
    fclose(f);
    int svr = idx("svr"), dac = idx("dac"), fft = idx("fft"), out = idx("out");
    long long s1 = count_paths(svr, dac) * count_paths(dac, fft) * count_paths(fft, out);
    long long s2 = count_paths(svr, fft) * count_paths(fft, dac) * count_paths(dac, out);
    printf("Paths (svr->dac->fft->out): %lld\n", s1);
    printf("Paths (svr->fft->dac->out): %lld\n", s2);
    printf("Total paths visiting both: %lld\n", s1 + s2);
    return 0;
}
