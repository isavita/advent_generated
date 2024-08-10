#import <Foundation/Foundation.h>

#define SIDE 5
#define SQUARE (SIDE * SIDE)

NSArray<NSNumber *> *parse() {
    NSMutableArray<NSNumber *> *res = [NSMutableArray arrayWithCapacity:SQUARE];
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    
    for (NSString *line in lines) {
        for (NSUInteger col = 0; col < SIDE; col++) {
            [res addObject:@([line characterAtIndex:col] == '#')];
        }
    }
    return res;
}

NSArray<NSNumber *> *next1(NSArray<NSNumber *> *grid) {
    NSMutableArray<NSNumber *> *newGrid = [NSMutableArray arrayWithCapacity:SQUARE];
    
    for (NSUInteger i = 0; i < SQUARE; i++) {
        NSUInteger row = i / SIDE, col = i % SIDE;
        NSUInteger neighbours = 0;

        if (row > 0 && [grid[i - SIDE] boolValue]) neighbours++;
        if (row < SIDE - 1 && [grid[i + SIDE] boolValue]) neighbours++;
        if (col > 0 && [grid[i - 1] boolValue]) neighbours++;
        if (col < SIDE - 1 && [grid[i + 1] boolValue]) neighbours++;

        if ([grid[i] boolValue] && neighbours != 1) {
            [newGrid addObject:@(NO)];
        } else if (![grid[i] boolValue] && (neighbours == 1 || neighbours == 2)) {
            [newGrid addObject:@(YES)];
        } else {
            [newGrid addObject:grid[i]];
        }
    }
    
    return newGrid;
}

NSInteger biodiversity(NSArray<NSNumber *> *grid) {
    NSInteger bio = 0;
    for (NSUInteger i = 0; i < SQUARE; i++) {
        if ([grid[i] boolValue]) bio += 1 << i;
    }
    return bio;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableSet *appeared = [NSMutableSet set];
        NSArray<NSNumber *> *grid = parse();
        [appeared addObject:grid];

        while (YES) {
            grid = next1(grid);
            if ([appeared containsObject:grid]) {
                NSLog(@"%ld", (long)biodiversity(grid));
                break;
            }
            [appeared addObject:grid];
        }
    }
    return 0;
}