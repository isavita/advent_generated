#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableArray *asteroids = [NSMutableArray array];
        for (int y = 0; y < lines.count; y++) {
            NSString *line = lines[y];
            for (int x = 0; x < line.length; x++) {
                if ([line characterAtIndex:x] == '#') {
                    [asteroids addObject:[NSValue valueWithPoint:NSMakePoint(x, y)]];
                }
            }
        }
        
        int maxAsteroids = 0;
        NSPoint bestLocation = NSZeroPoint;
        
        for (NSValue *asteroidValue in asteroids) {
            NSPoint asteroid = [asteroidValue pointValue];
            NSMutableSet *angles = [NSMutableSet set];
            
            for (NSValue *otherAsteroidValue in asteroids) {
                if (![asteroidValue isEqual:otherAsteroidValue]) {
                    NSPoint otherAsteroid = [otherAsteroidValue pointValue];
                    CGFloat angle = atan2(otherAsteroid.y - asteroid.y, otherAsteroid.x - asteroid.x);
                    [angles addObject:@(angle)];
                }
            }
            
            if (angles.count > maxAsteroids) {
                maxAsteroids = (int)angles.count;
                bestLocation = asteroid;
            }
        }
        
        printf("Best location for a new monitoring station: (%.0f, %.0f) with %d other asteroids detected.\n", bestLocation.x, bestLocation.y, maxAsteroids);
    }
    return 0;
}