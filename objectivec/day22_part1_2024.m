#import <Foundation/Foundation.h>

static inline uint64_t nextSecret(uint64_t s) {
    s ^= s * 64;
    s &= 0xFFFFFF;
    s ^= s / 32;
    s &= 0xFFFFFF;
    s ^= s * 2048;
    s &= 0xFFFFFF;
    return s;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *text = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray<NSString *> *lines = [text componentsSeparatedByCharactersInSet:
                                     [NSCharacterSet newlineCharacterSet]];
        uint64_t total = 0;
        for (NSString *line in lines) {
            if (line.length) {
                uint64_t s = strtoull(line.UTF8String, NULL, 10);
                for (int i = 0; i < 2000; ++i) s = nextSecret(s);
                total += s;
            }
        }
        printf("%llu\n", total);
    }
    return 0;
}