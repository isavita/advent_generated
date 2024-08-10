#import <Foundation/Foundation.h>

BOOL isLowPoint(NSArray<NSArray<NSNumber *> *> *heightmap, NSInteger x, NSInteger y) {
    NSInteger height = heightmap[y][x].integerValue;
    if (x > 0 && heightmap[y][x - 1].integerValue <= height) return NO;
    if (x < heightmap[y].count - 1 && heightmap[y][x + 1].integerValue <= height) return NO;
    if (y > 0 && heightmap[y - 1][x].integerValue <= height) return NO;
    if (y < heightmap.count - 1 && heightmap[y + 1][x].integerValue <= height) return NO;
    return YES;
}

NSInteger exploreBasin(NSArray<NSArray<NSNumber *> *> *heightmap, NSInteger x, NSInteger y, NSMutableSet *visited) {
    if ([visited containsObject:@[@(x), @(y)]] || heightmap[y][x].integerValue == 9) return 0;
    [visited addObject:@[@(x), @(y)]];
    NSInteger size = 1;
    NSArray *directions = @[@[@0, @-1], @[@-1, @0], @[@0, @1], @[@1, @0]];
    
    for (NSArray *dir in directions) {
        NSInteger newX = x + [dir[0] integerValue], newY = y + [dir[1] integerValue];
        if (newX >= 0 && newX < heightmap[0].count && newY >= 0 && newY < heightmap.count) {
            size += exploreBasin(heightmap, newX, newY, visited);
        }
    }
    return size;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) return 1;

        NSMutableArray<NSMutableArray<NSNumber *> *> *heightmap = [NSMutableArray array];
        for (NSString *line in [fileContents componentsSeparatedByString:@"\n"]) {
            if (line.length == 0) continue;
            NSMutableArray<NSNumber *> *row = [NSMutableArray array];
            for (NSUInteger i = 0; i < line.length; i++) {
                [row addObject:@([[line substringWithRange:NSMakeRange(i, 1)] integerValue])];
            }
            [heightmap addObject:row];
        }

        NSMutableArray<NSNumber *> *basinSizes = [NSMutableArray array];
        NSMutableSet *visited = [NSMutableSet set];
        
        for (NSUInteger y = 0; y < heightmap.count; y++) {
            for (NSUInteger x = 0; x < heightmap[y].count; x++) {
                if (isLowPoint(heightmap, x, y)) {
                    NSInteger size = exploreBasin(heightmap, x, y, visited);
                    [basinSizes addObject:@(size)];
                }
            }
        }

        [basinSizes sortUsingComparator:^NSComparisonResult(NSNumber *a, NSNumber *b) {
            return [b compare:a];
        }];

        NSInteger result = [basinSizes[0] integerValue] * [basinSizes[1] integerValue] * [basinSizes[2] integerValue];
        NSLog(@"%ld", (long)result);
    }
    return 0;
}