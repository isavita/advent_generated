#import <Foundation/Foundation.h>

#define NUM_STEPS 2000
#define PATTERN_COUNT 130321   // 19^4
#define MOD (1<<24)
#define MOD_MASK (MOD-1)

static inline unsigned next_secret(unsigned s) {
    unsigned x = s * 64;
    s ^= x;  s &= MOD_MASK;
    x = s / 32;
    s ^= x;  s &= MOD_MASK;
    x = s * 2048;
    s ^= x;  s &= MOD_MASK;
    return s;
}

static inline int encode4(int c1, int c2, int c3, int c4) {
    return (c1+9) + (c2+9)*19 + (c3+9)*361 + (c4+9)*6859;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                encoding:NSUTF8StringEncoding
                                                   error:&err];
        if (!txt) { NSLog(@"%@", err); return 1; }

        NSArray *lines = [txt componentsSeparatedByCharactersInSet:
                         [NSCharacterSet newlineCharacterSet]];
        NSMutableArray *buyers = [NSMutableArray array];
        for (NSString *l in lines) {
            NSInteger v = [l integerValue];
            if (l.length) [buyers addObject:@(v)];
        }

        long long global[PATTERN_COUNT];
        int local[PATTERN_COUNT];
        int prices[NUM_STEPS+1];
        int changes[NUM_STEPS];
        memset(global, 0, sizeof(global));

        for (NSNumber *n in buyers) {
            unsigned s = (unsigned)[n unsignedIntegerValue];
            for (int i = 0; i <= NUM_STEPS; ++i) {
                prices[i] = (int)(s % 10);
                s = next_secret(s);
            }
            for (int i = 0; i < NUM_STEPS; ++i) changes[i] = prices[i+1] - prices[i];

            memset(local, -1, sizeof(local));
            for (int i = 0; i <= NUM_STEPS - 4; ++i) {
                int c1 = changes[i], c2 = changes[i+1], c3 = changes[i+2], c4 = changes[i+3];
                if (abs(c1) > 9 || abs(c2) > 9 || abs(c3) > 9 || abs(c4) > 9) continue;
                int idx = encode4(c1,c2,c3,c4);
                if (local[idx] == -1) local[idx] = prices[i+4];
            }
            for (int i = 0; i < PATTERN_COUNT; ++i)
                if (local[i] != -1) global[i] += local[i];
        }

        long long best = 0;
        for (int i = 0; i < PATTERN_COUNT; ++i)
            if (global[i] > best) best = global[i];
        printf("%lld\n", best);
    }
    return 0;
}