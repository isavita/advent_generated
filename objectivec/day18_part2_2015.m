#import <Foundation/Foundation.h>

const int gridSize = 100;
const int steps = 100;

@interface Grid : NSObject {
    NSMutableArray *_grid;
}

- (instancetype)initWithContentsOfFile:(NSString *)path;
- (void)step;
- (int)countLightsOn;

@end

@implementation Grid

- (instancetype)initWithContentsOfFile:(NSString *)path {
    self = [super init];
    if (self) {
        NSError *error;
        NSString *contents = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return nil;
        }
        NSArray *lines = [contents componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        _grid = [NSMutableArray arrayWithCapacity:gridSize];
        for (int y = 0; y < gridSize; y++) {
            NSMutableArray *row = [NSMutableArray arrayWithCapacity:gridSize];
            NSString *line = lines[y];
            for (int x = 0; x < gridSize; x++) {
                [row addObject:@([line characterAtIndex:x] == '#')];
            }
            [_grid addObject:row];
        }
        [_grid[0] replaceObjectAtIndex:0 withObject:@YES];
        [_grid[0] replaceObjectAtIndex:gridSize-1 withObject:@YES];
        [_grid[gridSize-1] replaceObjectAtIndex:0 withObject:@YES];
        [_grid[gridSize-1] replaceObjectAtIndex:gridSize-1 withObject:@YES];
    }
    return self;
}

- (void)step {
    NSMutableArray *newGrid = [NSMutableArray arrayWithCapacity:gridSize];
    for (int y = 0; y < gridSize; y++) {
        NSMutableArray *row = [NSMutableArray arrayWithCapacity:gridSize];
        for (int x = 0; x < gridSize; x++) {
            int onNeighbors = 0;
            for (int dx = -1; dx <= 1; dx++) {
                for (int dy = -1; dy <= 1; dy++) {
                    if (dx == 0 && dy == 0) continue;
                    int nx = x + dx;
                    int ny = y + dy;
                    if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && [_grid[ny][nx] boolValue]) {
                        onNeighbors++;
                    }
                }
            }
            if ([_grid[y][x] boolValue]) {
                [row addObject:@(onNeighbors == 2 || onNeighbors == 3)];
            } else {
                [row addObject:@(onNeighbors == 3)];
            }
        }
        [newGrid addObject:row];
    }
    _grid = newGrid;
    [_grid[0] replaceObjectAtIndex:0 withObject:@YES];
    [_grid[0] replaceObjectAtIndex:gridSize-1 withObject:@YES];
    [_grid[gridSize-1] replaceObjectAtIndex:0 withObject:@YES];
    [_grid[gridSize-1] replaceObjectAtIndex:gridSize-1 withObject:@YES];
}

- (int)countLightsOn {
    int count = 0;
    for (int y = 0; y < gridSize; y++) {
        for (int x = 0; x < gridSize; x++) {
            if ([_grid[y][x] boolValue]) count++;
        }
    }
    return count;
}

@end

int main(int argc, char *argv[]) {
    @autoreleasepool {
        Grid *grid = [[Grid alloc] initWithContentsOfFile:@"input.txt"];
        for (int i = 0; i < steps; i++) {
            [grid step];
        }
        NSLog(@"%d", [grid countLightsOn]);
    }
    return 0;
}