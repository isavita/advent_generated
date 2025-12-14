
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* ---------- 128‑bit helpers ---------- */
typedef __uint128_t u128;

/* convert decimal string to u128 */
static u128 str_to_u128(const char *s)
{
    u128 v = 0;
    while (*s) {
        v = v * 10 + (u128)(*s - '0');
        ++s;
    }
    return v;
}

/* convert u128 to decimal string (caller frees) */
static char *u128_to_str(u128 v)
{
    char buf[50];
    int i = 49;
    buf[i] = '\0';
    if (v == 0) {
        buf[--i] = '0';
    } else {
        while (v) {
            buf[--i] = (char)('0' + (v % 10));
            v /= 10;
        }
    }
    return strdup(&buf[i]);
}

/* ---------- dynamic array of u128 ---------- */
typedef struct {
    u128 *data;
    size_t size;
    size_t cap;
} Vec;

static void vec_init(Vec *v)
{
    v->data = NULL;
    v->size = v->cap = 0;
}
static void vec_push(Vec *v, u128 x)
{
    if (v->size == v->cap) {
        v->cap = v->cap ? v->cap * 2 : 256;
        v->data = realloc(v->data, v->cap * sizeof(u128));
    }
    v->data[v->size++] = x;
}
static int cmp_u128(const void *a, const void *b)
{
    u128 av = *(const u128 *)a, bv = *(const u128 *)b;
    return (av > bv) - (av < bv);
}

/* ---------- power of 10 table (0..20) ---------- */
static u128 pow10[21];
static void init_pow10(void)
{
    pow10[0] = 1;
    for (int i = 1; i <= 20; ++i) pow10[i] = pow10[i - 1] * (u128)10;
}

/* ---------- main logic ---------- */
int main(void)
{
    /* read whole file */
    FILE *fp = fopen("input.txt", "rb");
    if (!fp) { perror("input.txt"); return 1; }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    char *buf = malloc(fsize + 1);
    fread(buf, 1, fsize, fp);
    buf[fsize] = '\0';
    fclose(fp);

    /* strip newlines and spaces */
    for (char *p = buf; *p; ++p)
        if (*p == '\n' || *p == '\r' || *p == ' ')
            *p = ',';
    /* now split by ',' */
    init_pow10();
    Vec ids;
    vec_init(&ids);

    char *saveptr1;
    char *token = strtok_r(buf, ",", &saveptr1);
    while (token) {
        if (*token) {
            char *dash = strchr(token, '-');
            if (!dash) { fprintf(stderr, "bad range %s\n", token); return 1; }
            *dash = '\0';
            u128 start = str_to_u128(token);
            u128 end   = str_to_u128(dash + 1);
            if (start > end) { u128 t = start; start = end; end = t; }

            /* examine half‑length k = 1 .. 10 (ID length 2k) */
            for (int k = 1; k <= 10; ++k) {
                u128 multiplier = pow10[k] + 1;               /* 10^k + 1 */
                u128 minSeed    = pow10[k - 1];               /* 10^{k-1} */
                u128 maxSeed    = pow10[k] - 1;               /* 10^k - 1 */

                /* sMin = ceil(start / multiplier) */
                u128 sMin = (start + multiplier - 1) / multiplier;
                /* sMax = floor(end / multiplier) */
                u128 sMax = end / multiplier;

                if (sMin < minSeed) sMin = minSeed;
                if (sMax > maxSeed) sMax = maxSeed;
                if (sMin > sMax) continue;

                for (u128 seed = sMin; seed <= sMax; ++seed) {
                    u128 id = seed * multiplier;
                    vec_push(&ids, id);
                }
            }
        }
        token = strtok_r(NULL, ",", &saveptr1);
    }
    free(buf);

    /* sort and keep unique */
    qsort(ids.data, ids.size, sizeof(u128), cmp_u128);
    u128 sum = 0;
    u128 prev = (u128)-1;
    for (size_t i = 0; i < ids.size; ++i) {
        if (ids.data[i] != prev) {
            sum += ids.data[i];
            prev = ids.data[i];
        }
    }

    char *out = u128_to_str(sum);
    printf("%s\n", out);
    free(out);
    free(ids.data);
    return 0;
}
