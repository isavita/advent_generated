#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSString *inputPath = @"input.txt";
        NSError *error = nil;
        NSString *inputString = [NSString stringWithContentsOfFile:inputPath encoding:NSUTF8StringEncoding error:&error];
        
        NSArray *positions = [inputString componentsSeparatedByString:@","];
        
        int minPosition = INT_MAX;
        int maxPosition = INT_MIN;
        
        for (NSString *position in positions) {
            int pos = [position intValue];
            if (pos < minPosition) {
                minPosition = pos;
            }
            if (pos > maxPosition) {
                maxPosition = pos;
            }
        }
        
        int minFuel = INT_MAX;
        int bestPosition = 0;
        
        for (int i = minPosition; i <= maxPosition; i++) {
            int fuel = 0;
            for (NSString *position in positions) {
                int pos = [position intValue];
                fuel += abs(pos - i);
            }
            if (fuel < minFuel) {
                minFuel = fuel;
                bestPosition = i;
            }
        }
        
        printf("The crabs should align at position %d with %d fuel.\n", bestPosition, minFuel);
    }
    
    return 0;
}