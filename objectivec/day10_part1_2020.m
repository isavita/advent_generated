#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSMutableArray *adapters = [NSMutableArray array];

        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            [adapters addObject:@([line intValue])];
        }

        NSArray *sortedAdapters = [adapters sortedArrayUsingSelector:@selector(compare:)];
        NSMutableDictionary *joltDifferences = [NSMutableDictionary dictionaryWithObject:@1 forKey:@3];
        int previousJoltage = 0;

        for (NSNumber *adapter in sortedAdapters) {
            int diff = [adapter intValue] - previousJoltage;
            int count = [[joltDifferences objectForKey:@(diff)] intValue] + 1;
            [joltDifferences setObject:@(count) forKey:@(diff)];
            previousJoltage = [adapter intValue];
        }

        int product = [[joltDifferences objectForKey:@1] intValue] * [[joltDifferences objectForKey:@3] intValue];
        printf("%d\n", product);
    }
    return 0;
}