
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    long long Min;
    long long Max;
} Range;

static int cmpRanges(const void *a, const void *b) {
    const Range *ra = (const Range *)a;
    const Range *rb = (const Range *)b;
    if (ra->Min < rb->Min) return -1;
    if (ra->Min > rb->Min) return 1;
    if (ra->Max < rb->Max) return -1;
    if (ra->Max > rb->Max) return 1;
    return 0;
}

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;

    Range *ranges = NULL;
    size_t cap = 0;
    size_t n = 0;

    char line[4096];
    while (fgets(line, sizeof(line), f)) {
        size_t len = strlen(line);
        char *p = line;
        while (*p && isspace((unsigned char)*p)) p++;
        if (*p == '\0') break;
        char *end = line + len - 1;
        while (end >= line && isspace((unsigned char)*end)) {
            *end = '\0';
            end--;
        }
        long long minVal, maxVal;
        if (sscanf(p, "%lld-%lld", &minVal, &maxVal) != 2) {
            fclose(f);
            return 1;
        }
        if (minVal > maxVal) {
            long long t = minVal;
            minVal = maxVal;
            maxVal = t;
        }
        if (n == cap) {
            cap = cap ? cap * 2 : 4;
            Range *tmp = realloc(ranges, cap * sizeof(Range));
            if (!tmp) {
                fclose(f);
                return 1;
            }
            ranges = tmp;
        }
        ranges[n].Min = minVal;
        ranges[n].Max = maxVal;
        n++;
    }

    fclose(f);

    if (n == 0) {
        printf("Total fresh IDs: 0\n");
        free(ranges);
        return 0;
    }

    qsort(ranges, n, sizeof(Range), cmpRanges);

    long long total = 0;
    long long currentMin = ranges[0].Min;
    long long currentMax = ranges[0].Max;

    for (size_t i = 1; i < n; i++) {
        Range next = ranges[i];
        if (next.Min <= currentMax) {
            if (next.Max > currentMax) currentMax = next.Max;
        } else {
            total += (currentMax - currentMin + 1);
            currentMin = next.Min;
            currentMax = next.Max;
        }
    }

    total += (currentMax - currentMin + 1);
    printf("Total fresh IDs: %lld\n", total);

    free(ranges);
    return 0;
}
