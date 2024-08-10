#import <Foundation/Foundation.h>

typedef struct {
    NSInteger x1, x2, y1, y2, z1, z2;
} Cuboid;

BOOL intersectsRegion(Cuboid cuboid, NSInteger min, NSInteger max) {
    return !(cuboid.x2 < min || cuboid.x1 > max ||
             cuboid.y2 < min || cuboid.y1 > max ||
             cuboid.z2 < min || cuboid.z1 > max);
}

NSInteger volume(Cuboid cuboid) {
    return (cuboid.x2 - cuboid.x1 + 1) *
           (cuboid.y2 - cuboid.y1 + 1) *
           (cuboid.z2 - cuboid.z1 + 1);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];

        NSMutableSet *onCubes = [NSMutableSet set];

        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" "];
            if ([parts count] < 2) continue;

            BOOL turnOn = [parts[0] isEqualToString:@"on"];
            NSString *coordinates = parts[1];
            NSArray *ranges = [coordinates componentsSeparatedByString:@","];

            NSInteger x1, x2, y1, y2, z1, z2;
            sscanf([ranges[0] UTF8String], "x=%ld..%ld", &x1, &x2);
            sscanf([ranges[1] UTF8String], "y=%ld..%ld", &y1, &y2);
            sscanf([ranges[2] UTF8String], "z=%ld..%ld", &z1, &z2);

            Cuboid cuboid = {x1, x2, y1, y2, z1, z2};

            if (intersectsRegion(cuboid, -50, 50)) {
                for (NSInteger x = MAX(cuboid.x1, -50); x <= MIN(cuboid.x2, 50); x++) {
                    for (NSInteger y = MAX(cuboid.y1, -50); y <= MIN(cuboid.y2, 50); y++) {
                        for (NSInteger z = MAX(cuboid.z1, -50); z <= MIN(cuboid.z2, 50); z++) {
                            NSString *key = [NSString stringWithFormat:@"%ld,%ld,%ld", x, y, z];
                            if (turnOn) {
                                [onCubes addObject:key];
                            } else {
                                [onCubes removeObject:key];
                            }
                        }
                    }
                }
            }
        }

        NSLog(@"Number of cubes on: %lu", (unsigned long)[onCubes count]);
    }
    return 0;
}