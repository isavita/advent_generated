#import <Foundation/Foundation.h>

typedef struct {
    long long x, y;
} Position;

typedef struct {
    long long vx, vy;
} Velocity;

typedef struct {
    Position position;
    Velocity velocity;
} Hailstone;

BOOL willIntersectInFuture(Hailstone h1, Hailstone h2, long long minXY, long long maxXY) {
    long long dx1 = h1.velocity.vx;
    long long dy1 = h1.velocity.vy;
    long long dx2 = h2.velocity.vx;
    long long dy2 = h2.velocity.vy;

    long long det = dx1 * dy2 - dx2 * dy1;
    if (det == 0) {
        return NO; // Lines are parallel
    }

    long long dx3 = h2.position.x - h1.position.x;
    long long dy3 = h2.position.y - h1.position.y;

    double t1 = (double)(dx3 * dy2 - dy3 * dx2) / det;
    double t2 = (double)(dx3 * dy1 - dy3 * dx1) / det;

    long long x = h1.position.x + t1 * dx1;
    long long y = h1.position.y + t1 * dy1;

    if (t1 >= 0 && t2 >= 0 && x >= minXY && x <= maxXY && y >= minXY && y <= maxXY) {
        return YES; // Intersection occurs within the test area in the future
    }

    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContent componentsSeparatedByString:@"\n"];

        NSMutableArray *hailstones = [NSMutableArray array];

        for (NSString *line in lines) {
            if (line.length == 0) continue;

            NSArray *parts = [line componentsSeparatedByString:@" @ "];
            NSArray *positionParts = [[parts[0] componentsSeparatedByString:@", "] subarrayWithRange:NSMakeRange(0, 2)];
            NSArray *velocityParts = [[parts[1] componentsSeparatedByString:@", "] subarrayWithRange:NSMakeRange(0, 2)];

            Hailstone hailstone;
            hailstone.position.x = [positionParts[0] longLongValue];
            hailstone.position.y = [positionParts[1] longLongValue];
            hailstone.velocity.vx = [velocityParts[0] longLongValue];
            hailstone.velocity.vy = [velocityParts[1] longLongValue];

            [hailstones addObject:[NSValue valueWithBytes:&hailstone objCType:@encode(Hailstone)]];
        }

        long long minXY = 200000000000000;
        long long maxXY = 400000000000000;
        int intersectionCount = 0;

        for (int i = 0; i < hailstones.count; i++) {
            for (int j = i + 1; j < hailstones.count; j++) {
                Hailstone h1, h2;
                [hailstones[i] getValue:&h1];
                [hailstones[j] getValue:&h2];

                if (willIntersectInFuture(h1, h2, minXY, maxXY)) {
                    intersectionCount++;
                }
            }
        }

        printf("Number of intersections within the test area: %d\n", intersectionCount);
    }
    return 0;
}