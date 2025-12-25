
#import <Foundation/Foundation.h>

static int rotations[24][3][3];

static void generateRotations() {
    int base[24][3][3] = {
        {{1,0,0},{0,1,0},{0,0,1}},{{1,0,0},{0,0,-1},{0,1,0}},{{1,0,0},{0,-1,0},{0,0,-1}},{{1,0,0},{0,0,1},{0,-1,0}},
        {{0,-1,0},{1,0,0},{0,0,1}},{{0,0,1},{1,0,0},{0,1,0}},{{0,1,0},{1,0,0},{0,0,-1}},{{0,0,-1},{1,0,0},{0,-1,0}},
        {{-1,0,0},{0,-1,0},{0,0,1}},{{-1,0,0},{0,0,-1},{0,-1,0}},{{-1,0,0},{0,1,0},{0,0,-1}},{{-1,0,0},{0,0,1},{0,1,0}},
        {{0,1,0},{-1,0,0},{0,0,1}},{{0,0,1},{-1,0,0},{0,-1,0}},{{0,-1,0},{-1,0,0},{0,0,-1}},{{0,0,-1},{-1,0,0},{0,1,0}},
        {{0,0,-1},{0,1,0},{1,0,0}},{{0,1,0},{0,0,1},{1,0,0}},{{0,0,1},{0,-1,0},{1,0,0}},{{0,-1,0},{0,0,-1},{1,0,0}},
        {{0,0,-1},{0,-1,0},{-1,0,0}},{{0,-1,0},{0,0,1},{-1,0,0}},{{0,0,1},{0,1,0},{-1,0,0}},{{0,1,0},{0,0,-1},{-1,0,0}}
    };
    memcpy(rotations, base, sizeof(rotations));
}

static NSArray *rotate(NSArray *p, int rot[3][3]) {
    int x = [p[0] intValue], y = [p[1] intValue], z = [p[2] intValue];
    return @[@(x*rot[0][0] + y*rot[0][1] + z*rot[0][2]),
             @(x*rot[1][0] + y*rot[1][1] + z*rot[1][2]),
             @(x*rot[2][0] + y*rot[2][1] + z*rot[2][2])];
}

static NSArray *add(NSArray *a, NSArray *b) {
    return @[@([a[0] intValue] + [b[0] intValue]),
             @([a[1] intValue] + [b[1] intValue]),
             @([a[2] intValue] + [b[2] intValue])];
}

static NSArray *subtract(NSArray *a, NSArray *b) {
    return @[@([a[0] intValue] - [b[0] intValue]),
             @([a[1] intValue] - [b[1] intValue]),
             @([a[2] intValue] - [b[2] intValue])];
}

static int manhattan(NSArray *a, NSArray *b) {
    return abs([a[0] intValue] - [b[0] intValue]) +
           abs([a[1] intValue] - [b[1] intValue]) +
           abs([a[2] intValue] - [b[2] intValue]);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        generateRotations();
        NSError *err = nil;
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&err];
        if (!data) return 1;
        NSArray *lines = [data componentsSeparatedByString:@"\n"];
        NSMutableArray *scanners = [NSMutableArray array];
        NSMutableArray *current = nil;
        for (NSString *line in lines) {
            NSString *trim = [line stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
            if ([trim hasPrefix:@"---"]) {
                current = [NSMutableArray array];
                [scanners addObject:current];
            } else if (trim.length > 0) {
                NSArray *parts = [trim componentsSeparatedByString:@","];
                [current addObject:@[parts[0], parts[1], parts[2]]];
            }
        }
        NSMutableSet *aligned = [NSMutableSet setWithObject:@0];
        NSMutableDictionary *scannerPos = [NSMutableDictionary dictionary];
        scannerPos[@0] = @[@0, @0, @0];
        NSMutableSet *beacons = [NSMutableSet set];
        for (NSArray *p in scanners[0]) {
            [beacons addObject:[NSString stringWithFormat:@"%@,%@,%@", p[0], p[1], p[2]]];
        }
        NSMutableIndexSet *pending = [NSMutableIndexSet indexSetWithIndexesInRange:NSMakeRange(1, scanners.count - 1)];
        while (pending.count) {
            BOOL found = NO;
            for (NSUInteger scanner = pending.firstIndex; scanner != NSNotFound; scanner = [pending indexGreaterThanIndex:scanner]) {
                for (int r = 0; r < 24; r++) {
                    NSMutableArray *rotated = [NSMutableArray array];
                    for (NSArray *p in scanners[scanner]) {
                        [rotated addObject:rotate(p, rotations[r])];
                    }
                    NSMutableDictionary *deltas = [NSMutableDictionary dictionary];
                    NSString *bestKey = nil;
                    int best = 0;
                    for (NSArray *beacon in rotated) {
                        for (NSString *s in beacons) {
                            NSArray *parts = [s componentsSeparatedByString:@","];
                            NSArray *delta = subtract(parts, beacon);
                            NSString *key = [NSString stringWithFormat:@"%@,%@,%@", delta[0], delta[1], delta[2]];
                            int count = [deltas[key] intValue] + 1;
                            deltas[key] = @(count);
                            if (count > best) {
                                best = count;
                                bestKey = key;
                            }
                        }
                    }
                    if (best >= 12) {
                        NSArray *dParts = [bestKey componentsSeparatedByString:@","];
                        scannerPos[@(scanner)] = dParts;
                        for (NSArray *beacon in rotated) {
                            NSArray *translated = add(beacon, dParts);
                            [beacons addObject:[NSString stringWithFormat:@"%@,%@,%@", translated[0], translated[1], translated[2]]];
                        }
                        [aligned addObject:@(scanner)];
                        [pending removeIndex:scanner];
                        found = YES;
                        break;
                    }
                }
                if (found) break;
            }
        }
        int maxDist = 0;
        for (NSNumber *i in scannerPos.allValues) {
            for (NSNumber *j in scannerPos.allValues) {
                int d = manhattan(i, j);
                if (d > maxDist) maxDist = d;
            }
        }
        printf("%d\n", maxDist);
    }
    return 0;
}
