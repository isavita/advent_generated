
#import <Foundation/Foundation.h>
typedef __uint128_t u128;

static u128 str_to_u128(const char *s) {
    u128 v = 0;
    while (*s) v = v * 10 + (u128)(*s - '0'), ++s;
    return v;
}
static char *u128_to_str(u128 v) {
    char buf[50];
    int i = 49;
    buf[i] = '\0';
    if (v == 0) buf[--i] = '0';
    else while (v) buf[--i] = (char)('0' + (v % 10)), v /= 10;
    return strdup(&buf[i]);
}
typedef struct { u128 *data; size_t size, cap; } Vec;
static void vec_init(Vec *v) { v->data = NULL; v->size = v->cap = 0; }
static void vec_push(Vec *v, u128 x) {
    if (v->size == v->cap) {
        v->cap = v->cap ? v->cap * 2 : 256;
        v->data = realloc(v->data, v->cap * sizeof(u128));
    }
    v->data[v->size++] = x;
}
static int cmp_u128(const void *a, const void *b) {
    u128 av = *(const u128 *)a, bv = *(const u128 *)b;
    return (av > bv) - (av < bv);
}
static u128 pow10[21];
static void init_pow10(void) {
    pow10[0] = 1;
    for (int i = 1; i <= 20; ++i) pow10[i] = pow10[i-1] * (u128)10;
}
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];
        if (!data) { perror("input.txt"); return 1; }
        char *buf = malloc(data.length + 1);
        memcpy(buf, data.bytes, data.length);
        buf[data.length] = '\0';
        for (char *p = buf; *p; ++p)
            if (*p == '\n' || *p == '\r' || *p == ' ') *p = ',';
        init_pow10();
        Vec ids; vec_init(&ids);
        char *saveptr; char *token = strtok_r(buf, ",", &saveptr);
        while (token) {
            if (*token) {
                char *dash = strchr(token, '-');
                if (!dash) { fprintf(stderr, "bad range %s\n", token); return 1; }
                *dash = '\0';
                u128 start = str_to_u128(token);
                u128 end   = str_to_u128(dash + 1);
                if (start > end) { u128 t = start; start = end; end = t; }
                for (int k = 1; k <= 10; ++k) {
                    u128 mult = pow10[k] + 1;
                    u128 minSeed = pow10[k-1];
                    u128 maxSeed = pow10[k] - 1;
                    u128 sMin = (start + mult - 1) / mult;
                    u128 sMax = end / mult;
                    if (sMin < minSeed) sMin = minSeed;
                    if (sMax > maxSeed) sMax = maxSeed;
                    if (sMin > sMax) continue;
                    for (u128 seed = sMin; seed <= sMax; ++seed) {
                        vec_push(&ids, seed * mult);
                    }
                }
            }
            token = strtok_r(NULL, ",", &saveptr);
        }
        free(buf);
        qsort(ids.data, ids.size, sizeof(u128), cmp_u128);
        u128 sum = 0, prev = (u128)-1;
        for (size_t i = 0; i < ids.size; ++i)
            if (ids.data[i] != prev) sum += ids.data[i], prev = ids.data[i];
        char *out = u128_to_str(sum);
        printf("%s\n", out);
        free(out);
        free(ids.data);
    }
    return 0;
}
