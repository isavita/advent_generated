#import <Foundation/Foundation.h>

@interface Cube : NSObject
@property (nonatomic) NSInteger x;
@property (nonatomic) NSInteger y;
@property (nonatomic) NSInteger z;
@end

@implementation Cube
@end

NSInteger calculateSurfaceArea(NSArray<Cube *> *cubes) {
    NSMutableSet<NSString *> *cubeSet = [NSMutableSet set];
    for (Cube *cube in cubes) {
        [cubeSet addObject:[NSString stringWithFormat:@"%ld,%ld,%ld", (long)cube.x, (long)cube.y, (long)cube.z]];
    }

    NSInteger surfaceArea = 0;
    for (Cube *cube in cubes) {
        NSArray<NSArray<NSNumber *> *> *neighbors = @[
            @[@(cube.x + 1), @(cube.y), @(cube.z)],
            @[@(cube.x - 1), @(cube.y), @(cube.z)],
            @[@(cube.x), @(cube.y + 1), @(cube.z)],
            @[@(cube.x), @(cube.y - 1), @(cube.z)],
            @[@(cube.x), @(cube.y), @(cube.z + 1)],
            @[@(cube.x), @(cube.y), @(cube.z - 1)]
        ];

        for (NSArray<NSNumber *> *neighbor in neighbors) {
            NSString *neighborKey = [NSString stringWithFormat:@"%@,%@,%@", neighbor[0], neighbor[1], neighbor[2]];
            if (![cubeSet containsObject:neighborKey]) {
                surfaceArea++;
            }
        }
    }

    return surfaceArea;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [fileContent componentsSeparatedByString:@"\n"];

        NSMutableArray<Cube *> *cubes = [NSMutableArray array];
        for (NSString *line in lines) {
            if (line.length > 0) {
                NSArray<NSString *> *components = [line componentsSeparatedByString:@","];
                Cube *cube = [[Cube alloc] init];
                cube.x = [components[0] integerValue];
                cube.y = [components[1] integerValue];
                cube.z = [components[2] integerValue];
                [cubes addObject:cube];
            }
        }

        NSInteger surfaceArea = calculateSurfaceArea(cubes);
        NSLog(@"Surface Area: %ld", (long)surfaceArea);
    }
    return 0;
}