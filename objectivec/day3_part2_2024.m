
#import <Foundation/Foundation.h>
#import <ctype.h>
#import <string.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];
        if (!data) return 1;
        const char *c = data.bytes;
        size_t n = data.length, i = 0;
        long sum = 0;
        BOOL enabled = YES;
        while (i < n) {
            if (i + 4 <= n && strncmp(c + i, "mul(", 4) == 0) {
                if (enabled) {
                    i += 4;
                    long a = 0;
                    while (i < n && isdigit(c[i])) a = a * 10 + (c[i++] - '0');
                    if (i >= n || c[i] != ',') { i++; continue; }
                    i++;
                    long b = 0;
                    while (i < n && isdigit(c[i])) b = b * 10 + (c[i++] - '0');
                    if (i >= n || c[i] != ')') { i++; continue; }
                    sum += a * b;
                }
                i++;
            } else if (i + 4 <= n && strncmp(c + i, "do()", 4) == 0) {
                enabled = YES;
                i += 4;
            } else if (i + 7 <= n && strncmp(c + i, "don't()", 7) == 0) {
                enabled = NO;
                i += 7;
            } else {
                i++;
            }
        }
        printf("%ld\n", sum);
    }
    return 0;
}
