#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *containers = [input componentsSeparatedByString:@"\n"];
        containers = [containers sortedArrayUsingSelector:@selector(compare:)];
        
        int target = 150;
        int count = 0;
        int minCount = INT_MAX;
        int ways = 0;
        
        for (int i = 1; i < (1 << containers.count); i++) {
            int sum = 0;
            int containerCount = 0;
            for (int j = 0; j < containers.count; j++) {
                if (i & (1 << j)) {
                    sum += [containers[j] intValue];
                    containerCount++;
                }
            }
            if (sum == target) {
                count++;
                if (containerCount < minCount) {
                    minCount = containerCount;
                    ways = 1;
                } else if (containerCount == minCount) {
                    ways++;
                }
            }
        }
        
        printf("Part One: %d\n", count);
        printf("Part Two: %d\n", ways);
    }
    return 0;
}