
#import <Foundation/Foundation.h>

static int strongest(int *p1, int *p2, int n, int cur, unsigned long long used) {
    int max = 0;
    for (int i = 0; i < n; ++i) {
        if (!(used & (1ULL << i))) {
            if (p1[i] == cur || p2[i] == cur) {
                int nxt = (p1[i] == cur) ? p2[i] : p1[i];
                int val = p1[i] + p2[i] + strongest(p1, p2, n, nxt, used | (1ULL << i));
                if (val > max) max = val;
            }
        }
    }
    return max;
}

static void longest(int *p1, int *p2, int n, int cur, unsigned long long used, int *bestLen, int *bestStr) {
    for (int i = 0; i < n; ++i) {
        if (!(used & (1ULL << i))) {
            if (p1[i] == cur || p2[i] == cur) {
                int nxt = (p1[i] == cur) ? p2[i] : p1[i];
                int len = 0, str = 0;
                longest(p1, p2, n, nxt, used | (1ULL << i), &len, &str);
                ++len;
                str += p1[i] + p2[i];
                if (len > *bestLen || (len == *bestLen && str > *bestStr)) {
                    *bestLen = len;
                    *bestStr = str;
                }
            }
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [input componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int count = (int)lines.count;
        int *p1 = calloc(count, sizeof(int));
        int *p2 = calloc(count, sizeof(int));
        int n = 0;
        for (NSString *line in lines) {
            NSArray<NSString *> *parts = [line componentsSeparatedByString:@"/"];
            if (parts.count == 2) {
                p1[n] = parts[0].intValue;
                p2[n] = parts[1].intValue;
                ++n;
            }
        }
        int strong = strongest(p1, p2, n, 0, 0);
        int bestLen = 0, bestStr = 0;
        longest(p1, p2, n, 0, 0, &bestLen, &bestStr);
        printf("Strongest bridge strength: %d\n", strong);
        printf("Longest bridge strength: %d\n", bestStr);
        free(p1);
        free(p2);
    }
    return 0;
}
