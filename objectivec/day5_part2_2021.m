#import <Foundation/Foundation.h>

int abs(int x) {
    return x < 0 ? -x : x;
}

int sign(int x) {
    return (x > 0) - (x < 0);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error;
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Failed to open input file: %@", error);
            return 1;
        }

        NSMutableArray<NSArray<NSNumber *> *> *lines = [NSMutableArray array];
        for (NSString *line in [content componentsSeparatedByString:@"\n"]) {
            if ([line length] == 0) continue;
            NSArray *parts = [line componentsSeparatedByString:@" -> "];
            NSArray *start = [parts[0] componentsSeparatedByString:@","];
            NSArray *end = [parts[1] componentsSeparatedByString:@","];

            int x1 = [start[0] intValue], y1 = [start[1] intValue];
            int x2 = [end[0] intValue], y2 = [end[1] intValue];

            [lines addObject:@[@(x1), @(y1), @(x2), @(y2)]];
        }

        NSMutableDictionary<NSString *, NSNumber *> *overlaps = [NSMutableDictionary dictionary];

        for (NSArray<NSNumber *> *line in lines) {
            int x1 = [line[0] intValue], y1 = [line[1] intValue];
            int x2 = [line[2] intValue], y2 = [line[3] intValue];

            int xStep = sign(x2 - x1);
            int yStep = sign(y2 - y1);
            int steps = abs(x2 - x1) + 1;
            if (abs(y2 - y1) > abs(x2 - x1)) {
                steps = abs(y2 - y1) + 1;
            }

            for (int i = 0; i < steps; i++) {
                NSString *pointKey = [NSString stringWithFormat:@"%d,%d", x1 + i * xStep, y1 + i * yStep];
                overlaps[pointKey] = @(overlaps[pointKey].intValue + 1);
            }
        }

        int count = 0;
        for (NSNumber *v in overlaps.allValues) {
            if (v.intValue > 1) count++;
        }

        NSLog(@"%d", count);
    }
    return 0;
}