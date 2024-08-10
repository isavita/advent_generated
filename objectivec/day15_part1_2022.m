#import <Foundation/Foundation.h>

// Define a structure to hold sensor and beacon coordinates
typedef struct {
    int sensorX;
    int sensorY;
    int beaconX;
    int beaconY;
} SensorBeacon;

// Function to calculate the Manhattan distance
int manhattanDistance(int x1, int y1, int x2, int y2) {
    return abs(x1 - x2) + abs(y1 - y2);
}

// Function to parse the input file and populate the sensor-beacon array
NSArray<NSValue *>* parseInput(NSString *filePath) {
    NSMutableArray<NSValue *> *sensorBeacons = [NSMutableArray array];
    NSError *error = nil;
    NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];

    if (error) {
        NSLog(@"Error reading file: %@", error);
        return nil;
    }

    NSArray *lines = [fileContent componentsSeparatedByString:@"\n"];
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)" options:0 error:&error];

    for (NSString *line in lines) {
        NSTextCheckingResult *match = [regex firstMatchInString:line options:0 range:NSMakeRange(0, [line length])];
        if (match) {
            SensorBeacon sb;
            sb.sensorX = [[line substringWithRange:[match rangeAtIndex:1]] intValue];
            sb.sensorY = [[line substringWithRange:[match rangeAtIndex:2]] intValue];
            sb.beaconX = [[line substringWithRange:[match rangeAtIndex:3]] intValue];
            sb.beaconY = [[line substringWithRange:[match rangeAtIndex:4]] intValue];
            [sensorBeacons addObject:[NSValue valueWithBytes:&sb objCType:@encode(SensorBeacon)]];
        }
    }

    return sensorBeacons;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSArray<NSValue *> *sensorBeacons = parseInput(filePath);

        if (!sensorBeacons) {
            return 1;
        }

        int targetY = 2000000;
        NSMutableSet<NSNumber *> *excludedPositions = [NSMutableSet set];

        for (NSValue *value in sensorBeacons) {
            SensorBeacon sb;
            [value getValue:&sb];

            int distance = manhattanDistance(sb.sensorX, sb.sensorY, sb.beaconX, sb.beaconY);
            int yDistance = abs(sb.sensorY - targetY);

            if (yDistance <= distance) {
                int xRange = distance - yDistance;
                for (int x = sb.sensorX - xRange; x <= sb.sensorX + xRange; x++) {
                    if (sb.beaconY != targetY || sb.beaconX != x) {
                        [excludedPositions addObject:@(x)];
                    }
                }
            }
        }

        NSLog(@"Number of positions that cannot contain a beacon: %lu", (unsigned long)[excludedPositions count]);
    }
    return 0;
}