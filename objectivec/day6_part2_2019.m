
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray *lines = [data componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableDictionary<NSString *, NSString *> *orbits = [NSMutableDictionary dictionary];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray *parts = [line componentsSeparatedByString:@")"];
            orbits[parts[1]] = parts[0];
        }

        long long total = 0;
        for (NSString *orbiter in orbits) {
            NSString *cur = orbiter;
            while (orbits[cur]) {
                cur = orbits[cur];
                total++;
            }
        }
        printf("%lld\n", total); // Part One

        NSMutableArray *youPath = [NSMutableArray array];
        NSString *cur = @"YOU";
        while (orbits[cur]) {
            cur = orbits[cur];
            [youPath addObject:cur];
        }

        NSMutableArray *sanPath = [NSMutableArray array];
        cur = @"SAN";
        while (orbits[cur]) {
            cur = orbits[cur];
            [sanPath addObject:cur];
        }

        NSSet *youSet = [NSSet setWithArray:youPath];
        NSString *common = nil;
        for (NSString *obj in sanPath) {
            if ([youSet containsObject:obj]) { common = obj; break; }
        }

        NSUInteger youIdx = [youPath indexOfObject:common];
        NSUInteger sanIdx = [sanPath indexOfObject:common];
        printf("%lu\n", (unsigned long)(youIdx + sanIdx)); // Part Two
    }
    return 0;
}
