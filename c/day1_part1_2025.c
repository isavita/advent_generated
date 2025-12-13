
#include <stdio.h>
#include <ctype.h>

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;

    int pos = 50, cnt = 0, amt;
    char dir, buf[32];

    while (fscanf(f, "%31s", buf) == 1) {
        dir = buf[0];
        sscanf(buf + 1, "%d", &amt);
        pos = (pos + (dir == 'R' ? amt : -amt)) % 100;
        if (pos < 0) pos += 100;
        cnt += (pos == 0);
    }
    fclose(f);
    printf("%d\n", cnt);
    return 0;
}
