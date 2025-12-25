
#import <Foundation/Foundation.h>

static NSMutableDictionary<NSString *, NSArray<NSNumber *> *> *memo;

NSArray<NSNumber *> *play(int p1, int p2, int s1, int s2, int rollsLeft, BOOL p1Turn) {
    NSString *key = [NSString stringWithFormat:@"%d,%d,%d,%d,%d,%d", p1, p2, s1, s2, rollsLeft, p1Turn];
    NSArray<NSNumber *> *cached = memo[key];
    if (cached) return cached;

    int idx = p1Turn ? 0 : 1;
    if (rollsLeft == 0) {
        if (idx == 0) s1 += p1; else s2 += p2;
        if (s1 >= 21) return @[@1, @0];
        if (s2 >= 21) return @[@0, @1];
        p1Turn = !p1Turn;
        rollsLeft = 3;
        idx = 1 - idx;
    }

    long w1 = 0, w2 = 0;
    for (int r = 1; r <= 3; r++) {
        int np1 = p1, np2 = p2;
        if (idx == 0) {
            np1 += r;
            if (np1 > 10) np1 -= 10;
        } else {
            np2 += r;
            if (np2 > 10) np2 -= 10;
        }
        NSArray<NSNumber *> *res = play(np1, np2, s1, s2, rollsLeft - 1, p1Turn);
        w1 += [res[0] longLongValue];
        w2 += [res[1] longLongValue];
    }
    NSArray<NSNumber *> *out = @[@(w1), @(w2)];
    memo[key] = out;
    return out;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [input componentsSeparatedByString:@"\n"];
        int pos1 = 0, pos2 = 0;
        sscanf([lines[0] UTF8String], "Player %*d starting position: %d", &pos1);
        sscanf([lines[1] UTF8String], "Player %*d starting position: %d", &pos2);
        memo = [NSMutableDictionary dictionary];
        NSArray<NSNumber *> *wins = play(pos1, pos2, 0, 0, 3, YES);
        long max = MAX([wins[0] longLongValue], [wins[1] longLongValue]);
        printf("%ld\n", max);
    }
    return 0;
}
