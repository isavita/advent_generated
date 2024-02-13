#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int earliestTimestamp = [lines[0] intValue];
        NSArray *busIDs = [lines[1] componentsSeparatedByString:@","];
        
        int minWaitTime = INT_MAX;
        int busID = 0;
        
        for (NSString *busIDString in busIDs) {
            if ([busIDString isEqualToString:@"x"]) {
                continue;
            }
            int currentBusID = [busIDString intValue];
            int timeToWait = currentBusID - (earliestTimestamp % currentBusID);
            if (timeToWait < minWaitTime) {
                minWaitTime = timeToWait;
                busID = currentBusID;
            }
        }
        
        printf("%d\n", busID * minWaitTime);
    }
    return 0;
}