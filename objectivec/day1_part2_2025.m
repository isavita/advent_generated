
#import <Foundation/Foundation.h>

static inline int floorDiv(int a, int b) {
    int q = a / b;
    if (a < 0 && a % b != 0) q--;
    return q;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *f = fopen("input.txt", "r");
        if (!f) return 1;
        int pos = 50, hits = 0, amt;
        char dir;
        while (fscanf(f, " %c%d ", &dir, &amt) == 2) {
            if (dir == 'R') {
                int n = pos + amt;
                hits += n / 100;
                pos = n % 100;
            } else {
                hits += floorDiv(pos - 1, 100) - floorDiv(pos - amt - 1, 100);
                pos = (pos - amt) % 100;
                if (pos < 0) pos += 100;
            }
        }
        fclose(f);
        printf("%d\n", hits);
    }
    return 0;
}
