
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char *springs;
    int *group;
    int group_len;
} Row;

typedef struct {
    int iSprings;
    int iGroup;
    int iContiguousDamaged;
} CacheKey;

typedef struct CacheEntry {
    CacheKey key;
    long long value;
    struct CacheEntry *next;
} CacheEntry;

#define CACHE_SIZE 131071

static CacheEntry *cache[CACHE_SIZE];

static unsigned long hash_key(CacheKey k) {
    unsigned long h = 17;
    h = h * 31 + k.iSprings;
    h = h * 31 + k.iGroup;
    h = h * 31 + k.iContiguousDamaged;
    return h % CACHE_SIZE;
}

static long long cache_get(CacheKey k) {
    unsigned long h = hash_key(k);
    for (CacheEntry *e = cache[h]; e; e = e->next) {
        if (e->key.iSprings == k.iSprings && e->key.iGroup == k.iGroup && e->key.iContiguousDamaged == k.iContiguousDamaged)
            return e->value;
    }
    return -1;
}

static void cache_put(CacheKey k, long long v) {
    unsigned long h = hash_key(k);
    CacheEntry *e = malloc(sizeof(CacheEntry));
    e->key = k;
    e->value = v;
    e->next = cache[h];
    cache[h] = e;
}

static Row parse_row(char *line) {
    Row r;
    char *sp = strchr(line, ' ');
    *sp = '\0';
    r.springs = strdup(line);
    char *grp = sp + 1;
    int cap = 8;
    r.group = malloc(cap * sizeof(int));
    r.group_len = 0;
    char *tok = strtok(grp, ",");
    while (tok) {
        if (r.group_len == cap) {
            cap <<= 1;
            r.group = realloc(r.group, cap * sizeof(int));
        }
        r.group[r.group_len++] = atoi(tok);
        tok = strtok(NULL, ",");
    }
    return r;
}

static Row unfold_row(Row r, int f) {
    Row nr;
    int sl = (int)strlen(r.springs);
    int nsl = sl * f + (f - 1);
    nr.springs = malloc(nsl + 1);
    int pos = 0;
    for (int i = 0; i < f; ++i) {
        memcpy(nr.springs + pos, r.springs, sl);
        pos += sl;
        if (i < f - 1) nr.springs[pos++] = '?';
    }
    nr.springs[nsl] = '\0';
    nr.group_len = r.group_len * f;
    nr.group = malloc(nr.group_len * sizeof(int));
    for (int i = 0; i < f; ++i) {
        memcpy(nr.group + i * r.group_len, r.group, r.group_len * sizeof(int));
    }
    return nr;
}

static long long rec(Row r, int iS, int iG, int iC) {
    if (iS == (int)strlen(r.springs)) {
        if (iG == r.group_len && iC == 0) return 1;
        if (iG == r.group_len - 1 && iC == r.group[iG]) return 1;
        return 0;
    }
    CacheKey k = {iS, iG, iC};
    long long cached = cache_get(k);
    if (cached != -1) return cached;
    long long res = 0;
    char c = r.springs[iS];
    if (c == '.' || c == '?') {
        if (iC == 0) res += rec(r, iS + 1, iG, 0);
        else if (iG < r.group_len && iC == r.group[iG]) res += rec(r, iS + 1, iG + 1, 0);
    }
    if (c == '#' || c == '?') {
        if (iG < r.group_len && iC < r.group[iG]) res += rec(r, iS + 1, iG, iC + 1);
    }
    cache_put(k, res);
    return res;
}

static long long count_arrangements(Row r) {
    for (int i = 0; i < CACHE_SIZE; ++i) cache[i] = NULL;
    long long v = rec(r, 0, 0, 0);
    for (int i = 0; i < CACHE_SIZE; ++i) {
        CacheEntry *e = cache[i];
        while (e) {
            CacheEntry *n = e->next;
            free(e);
            e = n;
        }
    }
    return v;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *f = fopen("input.txt", "r");
        if (!f) return 1;
        char *line = NULL;
        size_t cap = 0;
        long long total = 0;
        while (getline(&line, &cap, f) != -1) {
            size_t len = strlen(line);
            if (len && line[len-1] == '\n') line[len-1] = '\0';
            Row r = parse_row(line);
            Row ur = unfold_row(r, 5);
            total += count_arrangements(ur);
            free(r.springs);
            free(r.group);
            free(ur.springs);
            free(ur.group);
        }
        free(line);
        fclose(f);
        printf("%lld\n", total);
    }
    return 0;
}
