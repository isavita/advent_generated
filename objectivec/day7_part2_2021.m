#import <Foundation/Foundation.h>

int calculateNewFuel(int currentPosition, int newPosition) {
    int diff = abs(currentPosition - newPosition);
    return (diff * (diff + 1)) / 2;
}

int abs(int n) {
    if (n < 0) {
        return -n;
    }
    return n;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"%@", error);
            return 1;
        }
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSMutableArray *positions = [NSMutableArray array];
        
        for (NSString *line in lines) {
            NSArray *numbers = [line componentsSeparatedByString:@","];
            for (NSString *num_str in numbers) {
                int num = [num_str intValue];
                [positions addObject:@(num)];
            }
        }
        
        NSArray *sortedPositions = [positions sortedArrayUsingSelector:@selector(compare:)];
        
        int min_fuel = INT_MAX;
        for (int i = [sortedPositions[0] intValue]; i <= [sortedPositions[sortedPositions.count - 1] intValue]; i++) {
            int fuel = 0;
            for (NSNumber *pos in sortedPositions) {
                fuel += calculateNewFuel([pos intValue], i);
            }
            if (fuel < min_fuel) {
                min_fuel = fuel;
            }
        }
        
        printf("%d\n", min_fuel);
    }
    return 0;
}