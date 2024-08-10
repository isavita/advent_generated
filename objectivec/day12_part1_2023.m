#import <Foundation/Foundation.h>

// Function to count the number of valid arrangements for a given row
NSUInteger countValidArrangements(NSString *springs, NSArray<NSNumber *> *groups) {
    NSUInteger count = 0;
    NSUInteger springLength = springs.length;
    NSUInteger groupCount = groups.count;
    NSUInteger totalDamagedSprings = 0;

    for (NSNumber *group in groups) {
        totalDamagedSprings += group.unsignedIntegerValue;
    }

    NSMutableArray<NSNumber *> *positions = [NSMutableArray arrayWithCapacity:springLength];
    for (NSUInteger i = 0; i < springLength; i++) {
        if ([springs characterAtIndex:i] == '?') {
            [positions addObject:@(i)];
        }
    }

    NSUInteger positionsCount = positions.count;
    NSUInteger max = 1 << positionsCount;

    for (NSUInteger mask = 0; mask < max; mask++) {
        NSMutableString *currentSprings = [springs mutableCopy];
        for (NSUInteger i = 0; i < positionsCount; i++) {
            if (mask & (1 << i)) {
                [currentSprings replaceCharactersInRange:NSMakeRange(positions[i].unsignedIntegerValue, 1) withString:@"#"];
            } else {
                [currentSprings replaceCharactersInRange:NSMakeRange(positions[i].unsignedIntegerValue, 1) withString:@"."];
            }
        }

        NSArray<NSString *> *parts = [currentSprings componentsSeparatedByString:@"."];
        NSMutableArray<NSNumber *> *currentGroups = [NSMutableArray array];

        for (NSString *part in parts) {
            if (part.length > 0) {
                [currentGroups addObject:@(part.length)];
            }
        }

        if (currentGroups.count == groupCount) {
            BOOL valid = YES;
            for (NSUInteger i = 0; i < groupCount; i++) {
                if (![currentGroups[i] isEqualToNumber:groups[i]]) {
                    valid = NO;
                    break;
                }
            }
            if (valid) {
                count++;
            }
        }
    }

    return count;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];

        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }

        NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSUInteger totalArrangements = 0;

        for (NSString *line in lines) {
            if (line.length == 0) continue;

            NSArray<NSString *> *parts = [line componentsSeparatedByString:@" "];
            NSString *springs = parts[0];
            NSArray<NSString *> *groupStrings = [parts[1] componentsSeparatedByString:@","];
            NSMutableArray<NSNumber *> *groups = [NSMutableArray array];

            for (NSString *groupString in groupStrings) {
                [groups addObject:@(groupString.integerValue)];
            }

            totalArrangements += countValidArrangements(springs, groups);
        }

        NSLog(@"Total number of arrangements: %lu", (unsigned long)totalArrangements);
    }
    return 0;
}