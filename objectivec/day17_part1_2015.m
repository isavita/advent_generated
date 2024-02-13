#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *containers = [input componentsSeparatedByString:@"\n"];
        
        NSMutableArray *containerSizes = [NSMutableArray array];
        for (NSString *container in containers) {
            [containerSizes addObject:@(container.intValue)];
        }
        
        int target = 150;
        int count = 0;
        
        for (int i = 1; i < (1 << containerSizes.count); i++) {
            int sum = 0;
            for (int j = 0; j < containerSizes.count; j++) {
                if (i & (1 << j)) {
                    sum += [containerSizes[j] intValue];
                }
            }
            if (sum == target) {
                count++;
            }
        }
        
        printf("%d\n", count);
    }
    return 0;
}