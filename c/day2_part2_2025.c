
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef unsigned long long u64;

static int is_invalid(u64 x) {
    // Invalid if decimal representation is a repetition of a shorter block
    // repeated at least twice. No leading zeros exist in IDs by statement.
    char s[32];
    int n = snprintf(s, sizeof(s), "%llu", x);
    if (n <= 1) return 0;

    for (int p = 1; p <= n / 2; p++) {
        if (n % p) continue;
        int k = n / p;
        if (k < 2) continue;

        int ok = 1;
        for (int i = p; i < n && ok; i++) {
            if (s[i] != s[i % p]) ok = 0;
        }
        if (ok) return 1;
    }
    return 0;
}

int main(void) {
    FILE *f = fopen("input.txt", "rb");
    if (!f) return 1;

    // Read whole file
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (sz < 0) { fclose(f); return 1; }

    char *buf = (char*)malloc((size_t)sz + 1);
    if (!buf) { fclose(f); return 1; }
    size_t rd = fread(buf, 1, (size_t)sz, f);
    fclose(f);
    buf[rd] = '\0';

    u64 sum = 0;

    // Parse ranges "a-b" separated by commas/whitespace/newlines
    char *p = buf;
    while (*p) {
        while (*p == ' ' || *p == '\n' || *p == '\r' || *p == '\t' || *p == ',') p++;
        if (!*p) break;

        char *end1;
        u64 a = strtoull(p, &end1, 10);
        if (end1 == p || *end1 != '-') break;

        char *end2;
        u64 b = strtoull(end1 + 1, &end2, 10);
        if (end2 == end1 + 1) break;

        if (a > b) { u64 t = a; a = b; b = t; }

        for (u64 x = a; x <= b; x++) {
            if (is_invalid(x)) sum += x;
            if (x == UINT64_MAX) break; // safety against overflow in x++
        }

        p = end2;
    }

    free(buf);
    printf("%llu\n", sum);
    return 0;
}
