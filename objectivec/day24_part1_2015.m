
#import <Foundation/Foundation.h>
#import <limits.h>
#import <stdint.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:
                                      [NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSNumber *> *packages = [NSMutableArray array];
        int totalWeight = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            int v = line.intValue;
            [packages addObject:@(v)];
            totalWeight += v;
        }
        int num = (int)packages.count;
        int target = totalWeight / 3;
        uint64_t bestQE = UINT64_MAX;
        int bestLen = INT_MAX;
        int limit = 1 << num;
        for (int comb = 1; comb < limit; ++comb) {
            int w = 0;
            uint64_t qe = 1;
            int len = 0;
            for (int i = 0; i < num; ++i) {
                if (comb & (1 << i)) {
                    int val = packages[i].intValue;
                    w += val;
                    qe *= (uint64_t)val;
                    ++len;
                }
            }
            if (w == target && len <= bestLen) {
                if (len < bestLen || qe < bestQE) {
                    bestLen = len;
                    bestQE = qe;
                }
            }
        }
        printf("%llu\n", bestQE);
    }
    return 0;
}
