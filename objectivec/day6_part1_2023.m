#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];

        // Split the input into lines
        NSArray<NSString *> *lines = [fileContent componentsSeparatedByString:@"\n"];

        // Extract times and distances
        NSArray<NSString *> *timeParts = [[lines[0] componentsSeparatedByString:@":"][1] componentsSeparatedByString:@" "];
        NSArray<NSString *> *distanceParts = [[lines[1] componentsSeparatedByString:@":"][1] componentsSeparatedByString:@" "];

        NSMutableArray<NSNumber *> *times = [NSMutableArray array];
        NSMutableArray<NSNumber *> *distances = [NSMutableArray array];

        for (NSString *part in timeParts) {
            if (![part isEqualToString:@""]) {
                [times addObject:@([part intValue])];
            }
        }

        for (NSString *part in distanceParts) {
            if (![part isEqualToString:@""]) {
                [distances addObject:@([part intValue])];
            }
        }

        // Calculate the number of ways to beat the record for each race
        int product = 1;
        for (NSUInteger i = 0; i < times.count; i++) {
            int time = [times[i] intValue];
            int recordDistance = [distances[i] intValue];

            int waysToWin = 0;
            for (int holdTime = 0; holdTime <= time; holdTime++) {
                int travelTime = time - holdTime;
                int distance = holdTime * travelTime;
                if (distance > recordDistance) {
                    waysToWin++;
                }
            }
            product *= waysToWin;
        }

        // Print the result
        printf("%d\n", product);
    }
    return 0;
}