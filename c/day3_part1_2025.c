
#include <stdio.h>
#include <ctype.h>
#include <string.h>

int calc(const char *s) {
    int len = strlen(s);
    for (int d1 = 9; d1 >= 0; --d1) {
        char *p = strchr(s, '0' + d1);
        if (!p || p == s + len - 1) continue;
        int max2 = -1;
        while (*++p) {
            if (isdigit((unsigned char)*p)) {
                int v = *p - '0';
                if (v > max2) max2 = v;
                if (max2 == 9) break;
            }
        }
        if (max2 != -1) return d1 * 10 + max2;
    }
    return 0;
}

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;
    char line[256];
    int total = 0;
    while (fgets(line, sizeof(line), f)) {
        char *e = strchr(line, '\n');
        if (e) *e = '\0';
        total += calc(line);
    }
    fclose(f);
    printf("%d\n", total);
    return 0;
}
