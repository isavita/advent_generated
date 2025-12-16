#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

typedef struct { long long min, max; } Range;

static char *trim(char *s) {
    while (*s && isspace((unsigned char)*s)) s++;
    if (!*s) return s;
    char *e = s + strlen(s) - 1;
    while (e > s && isspace((unsigned char)*e)) *e-- = 0;
    return s;
}

static int cmp_range(const void *a, const void *b) {
    const Range *A = a;
    const Range *B = b;
    if (A->min < B->min) return -1;
    if (A->min > B->min) return 1;
    if (A->max < B->max) return -1;
    if (A->max > B->max) return 1;
    return 0;
}

static int contains(Range *arr, size_t n, long long x) {
    size_t l = 0, r = n;
    while (l < r) {
        size_t m = (l + r) >> 1;
        if (x < arr[m].min) r = m;
        else if (x > arr[m].max) l = m + 1;
        else return 1;
    }
    return 0;
}

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;
    char *line = NULL;
    size_t llen = 0;
    ssize_t nread;
    Range *ranges = NULL;
    size_t rcap = 0, rcount = 0;
    int parsingRanges = 1;
    long long freshCount = 0;
    while ((nread = getline(&line, &llen, f)) != -1) {
        char *s = trim(line);
        if (s[0] == '\0') {
            if (parsingRanges) {
                parsingRanges = 0;
                if (rcount > 0) {
                    qsort(ranges, rcount, sizeof(Range), cmp_range);
                    Range *merged = malloc(rcount * sizeof(Range));
                    size_t m = 0;
                    for (size_t i = 0; i < rcount; ++i) {
                        if (m == 0 || ranges[i].min > merged[m-1].max) {
                            merged[m++] = ranges[i];
                        } else if (ranges[i].max > merged[m-1].max) {
                            merged[m-1].max = ranges[i].max;
                        }
                    }
                    free(ranges);
                    ranges = merged;
                    rcount = m;
                    rcap = m;
                }
                continue;
            } else continue;
        }
        if (parsingRanges) {
            char *dash = strchr(s, '-');
            if (!dash) { free(line); free(ranges); fclose(f); return 1; }
            *dash = 0;
            char *a = trim(s);
            char *b = trim(dash + 1);
            char *ep;
            errno = 0;
            long long mn = strtoll(a, &ep, 10);
            if (ep == a || errno) { free(line); free(ranges); fclose(f); return 1; }
            errno = 0;
            long long mx = strtoll(b, &ep, 10);
            if (ep == b || errno) { free(line); free(ranges); fclose(f); return 1; }
            if (rcount == rcap) {
                size_t nc = rcap ? rcap * 2 : 16;
                Range *tmp = realloc(ranges, nc * sizeof(Range));
                if (!tmp) { free(line); free(ranges); fclose(f); return 1; }
                ranges = tmp;
                rcap = nc;
            }
            ranges[rcount].min = mn;
            ranges[rcount].max = mx;
            rcount++;
        } else {
            char *ep;
            errno = 0;
            long long id = strtoll(s, &ep, 10);
            if (ep == s || errno) { free(line); free(ranges); fclose(f); return 1; }
            if (rcount > 0 && contains(ranges, rcount, id)) freshCount++;
        }
    }
    free(line);
    fclose(f);
    printf("Number of fresh ingredients: %lld\n", freshCount);
    free(ranges);
    return 0;
}