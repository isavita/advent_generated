
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

static int isInvalid(uint64_t x) {
    char s[32];
    int n = snprintf(s, sizeof s, "%llu", x);
    if (n <= 1) return 0;
    for (int p = 1; p <= n / 2; ++p) {
        if (n % p) continue;
        int k = n / p;
        if (k < 2) continue;
        int ok = 1;
        for (int i = p; i < n && ok; ++i) {
            if (s[i] != s[i % p]) ok = 0;
        }
        if (ok) return 1;
    }
    return 0;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];
        if (!data) return 1;
        size_t sz = [data length];
        char *buf = malloc(sz + 1);
        if (!buf) return 1;
        memcpy(buf, [data bytes], sz);
        buf[sz] = '\0';
        uint64_t sum = 0;
        char *p = buf;
        while (*p) {
            while (*p==' '||*p=='\n'||*p=='\r'||*p=='\t'||*p==',') ++p;
            if (!*p) break;
            char *e1;
            uint64_t a = strtoull(p, &e1, 10);
            if (e1==p || *e1!='-') break;
            char *e2;
            uint64_t b = strtoull(e1+1, &e2, 10);
            if (e2==e1+1) break;
            if (a>b) { uint64_t t=a; a=b; b=t; }
            for (uint64_t x=a; x<=b; ++x) {
                if (isInvalid(x)) sum+=x;
                if (x==UINT64_MAX) break;
            }
            p = e2;
        }
        free(buf);
        printf("%llu\n", sum);
    }
    return 0;
}
