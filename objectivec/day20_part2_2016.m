#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableArray *ranges = [NSMutableArray array];
        
        for (NSString *line in lines) {
            NSArray *rangeComponents = [line componentsSeparatedByString:@"-"];
            NSInteger start = [rangeComponents[0] integerValue];
            NSInteger end = [rangeComponents[1] integerValue];
            NSIndexSet *rangeSet = [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(start, end - start + 1)];
            [ranges addObject:rangeSet];
        }
        
        NSMutableIndexSet *allowedIPs = [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(0, 4294967295)];
        
        for (NSIndexSet *rangeSet in ranges) {
            [allowedIPs removeIndexes:rangeSet];
        }
        
        printf("The lowest-valued IP that is not blocked is %ld\n", [allowedIPs firstIndex]);
        printf("The number of allowed IPs is %ld\n", [allowedIPs count]);
    }
    return 0;
}