#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableArray *blockedRanges = [NSMutableArray array];
        
        for (NSString *line in lines) {
            NSArray *rangeComponents = [line componentsSeparatedByString:@"-"];
            if (rangeComponents.count == 2) {
                NSUInteger start = [rangeComponents[0] integerValue];
                NSUInteger end = [rangeComponents[1] integerValue];
                NSIndexSet *rangeSet = [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(start, end - start + 1)];
                [blockedRanges addObject:rangeSet];
            }
        }
        
        NSMutableIndexSet *blockedIPs = [NSMutableIndexSet indexSet];
        
        for (NSIndexSet *rangeSet in blockedRanges) {
            [blockedIPs addIndexes:rangeSet];
        }
        
        NSUInteger lowestAllowedIP = [blockedIPs firstIndex];
        
        while ([blockedIPs containsIndex:lowestAllowedIP]) {
            lowestAllowedIP++;
        }
        
        printf("The lowest-valued IP that is not blocked is: %lu\n", lowestAllowedIP);
    }
    return 0;
}