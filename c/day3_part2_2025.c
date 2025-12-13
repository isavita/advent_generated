
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define TARGET 12
#define MAXL 10000
#define MAXD 1000

char buf[MAXL];
char stack[MAXL];

void add_big(char *sum, const char *num) {
    int i, carry = 0, al = strlen(sum), bl = strlen(num);
    for (i = 0; i < al || i < bl || carry; i++) {
        int d1 = (i < al) ? sum[al - 1 - i] - '0' : 0;
        int d2 = (i < bl) ? (i < bl ? num[bl - 1 - i] - '0' : 0) : 0;
        int t = d1 + d2 + carry;
        carry = t / 10;
        if (i < al) sum[al - 1 - i] = t % 10 + '0';
        else {
            memmove(sum + 1, sum, al + 1);
            sum[0] = t % 10 + '0';
            al++;
        }
    }
}

int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;
    char total[MAXD] = "0";
    while (fgets(buf, sizeof(buf), f)) {
        int n = 0, m = 0, len = strlen(buf);
        while (len && (buf[len - 1] < '0' || buf[len - 1] > '9')) buf[--len] = 0;
        if (len < TARGET) continue;
        int rem = len - TARGET;
        for (int i = 0; i < len; i++) {
            while (rem && m && stack[m - 1] < buf[i]) { m--; rem--; }
            stack[m++] = buf[i];
        }
        stack[TARGET] = 0;
        add_big(total, stack);
    }
    fclose(f);
    printf("%s\n", total);
    return 0;
}
