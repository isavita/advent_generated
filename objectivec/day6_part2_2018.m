#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *coordinates = [input componentsSeparatedByString:@"\n"];
        
        NSMutableArray *xCoordinates = [NSMutableArray array];
        NSMutableArray *yCoordinates = [NSMutableArray array];
        
        for (NSString *coordinate in coordinates) {
            NSArray *components = [coordinate componentsSeparatedByString:@", "];
            [xCoordinates addObject:@([components[0] intValue])];
            [yCoordinates addObject:@([components[1] intValue])];
        }
        
        NSInteger maxX = [[xCoordinates valueForKeyPath:@"@max.intValue"] integerValue];
        NSInteger maxY = [[yCoordinates valueForKeyPath:@"@max.intValue"] integerValue];
        
        NSInteger regionSize = 0;
        
        for (NSInteger x = 0; x <= maxX; x++) {
            for (NSInteger y = 0; y <= maxY; y++) {
                NSInteger totalDistance = 0;
                
                for (NSInteger i = 0; i < coordinates.count; i++) {
                    NSInteger xCoordinate = [xCoordinates[i] integerValue];
                    NSInteger yCoordinate = [yCoordinates[i] integerValue];
                    
                    totalDistance += labs(x - xCoordinate) + labs(y - yCoordinate);
                }
                
                if (totalDistance < 10000) {
                    regionSize++;
                }
            }
        }
        
        printf("%ld\n", regionSize);
    }
    return 0;
}