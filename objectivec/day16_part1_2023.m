#import <Foundation/Foundation.h>

typedef NS_ENUM(NSUInteger, Direction) {
    DirectionUp,
    DirectionDown,
    DirectionLeft,
    DirectionRight
};

typedef NS_ENUM(NSUInteger, TileType) {
    TileEmpty,
    TileMirrorForward,
    TileMirrorBackward,
    TileSplitterVertical,
    TileSplitterHorizontal
};

@interface Beam : NSObject
@property (nonatomic) NSInteger x;
@property (nonatomic) NSInteger y;
@property (nonatomic) Direction direction;
@end

@implementation Beam
@end

@interface Grid : NSObject
@property (nonatomic, strong) NSMutableArray<NSMutableArray<NSNumber *> *> *tiles;
@property (nonatomic, strong) NSMutableArray<NSMutableArray<NSNumber *> *> *energized;
@end

@implementation Grid

- (instancetype)initWithFile:(NSString *)filePath {
    self = [super init];
    if (self) {
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [fileContent componentsSeparatedByString:@"\n"];
        _tiles = [NSMutableArray array];
        _energized = [NSMutableArray array];

        for (NSString *line in lines) {
            NSMutableArray<NSNumber *> *tileRow = [NSMutableArray array];
            NSMutableArray<NSNumber *> *energizedRow = [NSMutableArray array];

            for (NSUInteger i = 0; i < line.length; i++) {
                unichar c = [line characterAtIndex:i];
                TileType tileType;
                switch (c) {
                    case '.': tileType = TileEmpty; break;
                    case '/': tileType = TileMirrorForward; break;
                    case '\\': tileType = TileMirrorBackward; break;
                    case '|': tileType = TileSplitterVertical; break;
                    case '-': tileType = TileSplitterHorizontal; break;
                    default: tileType = TileEmpty; break;
                }
                [tileRow addObject:@(tileType)];
                [energizedRow addObject:@(NO)];
            }

            [_tiles addObject:tileRow];
            [_energized addObject:energizedRow];
        }
    }
    return self;
}

- (void)energizeTileAtX:(NSInteger)x y:(NSInteger)y {
    if (x >= 0 && x < self.tiles.count && y >= 0 && y < self.tiles[0].count) {
        self.energized[x][y] = @(YES);
    }
}

- (NSInteger)countEnergizedTiles {
    NSInteger count = 0;
    for (NSArray<NSNumber *> *row in self.energized) {
        for (NSNumber *energized in row) {
            if ([energized boolValue]) {
                count++;
            }
        }
    }
    return count;
}

@end

void simulateBeam(Grid *grid) {
    NSMutableSet<NSString *> *visited = [NSMutableSet set];
    NSMutableArray<Beam *> *beams = [NSMutableArray array];

    Beam *initialBeam = [[Beam alloc] init];
    initialBeam.x = 0;
    initialBeam.y = 0;
    initialBeam.direction = DirectionRight;
    [beams addObject:initialBeam];

    while (beams.count > 0) {
        Beam *beam = beams.lastObject;
        [beams removeLastObject];

        NSString *key = [NSString stringWithFormat:@"%ld,%ld,%ld", beam.x, beam.y, beam.direction];
        if ([visited containsObject:key]) {
            continue;
        }
        [visited addObject:key];

        [grid energizeTileAtX:beam.x y:beam.y];

        TileType tileType = [grid.tiles[beam.x][beam.y] integerValue];
        switch (tileType) {
            case TileEmpty:
                switch (beam.direction) {
                    case DirectionUp: beam.x--; break;
                    case DirectionDown: beam.x++; break;
                    case DirectionLeft: beam.y--; break;
                    case DirectionRight: beam.y++; break;
                }
                break;
            case TileMirrorForward:
                switch (beam.direction) {
                    case DirectionUp: beam.direction = DirectionRight; beam.y++; break;
                    case DirectionDown: beam.direction = DirectionLeft; beam.y--; break;
                    case DirectionLeft: beam.direction = DirectionDown; beam.x++; break;
                    case DirectionRight: beam.direction = DirectionUp; beam.x--; break;
                }
                break;
            case TileMirrorBackward:
                switch (beam.direction) {
                    case DirectionUp: beam.direction = DirectionLeft; beam.y--; break;
                    case DirectionDown: beam.direction = DirectionRight; beam.y++; break;
                    case DirectionLeft: beam.direction = DirectionUp; beam.x--; break;
                    case DirectionRight: beam.direction = DirectionDown; beam.x++; break;
                }
                break;
            case TileSplitterVertical:
                if (beam.direction == DirectionLeft || beam.direction == DirectionRight) {
                    Beam *newBeam1 = [[Beam alloc] init];
                    newBeam1.x = beam.x;
                    newBeam1.y = beam.y;
                    newBeam1.direction = DirectionUp;
                    [beams addObject:newBeam1];

                    Beam *newBeam2 = [[Beam alloc] init];
                    newBeam2.x = beam.x;
                    newBeam2.y = beam.y;
                    newBeam2.direction = DirectionDown;
                    beam = newBeam2;
                } else {
                    switch (beam.direction) {
                        case DirectionUp: beam.x--; break;
                        case DirectionDown: beam.x++; break;
                        default: break;
                    }
                }
                break;
            case TileSplitterHorizontal:
                if (beam.direction == DirectionUp || beam.direction == DirectionDown) {
                    Beam *newBeam1 = [[Beam alloc] init];
                    newBeam1.x = beam.x;
                    newBeam1.y = beam.y;
                    newBeam1.direction = DirectionLeft;
                    [beams addObject:newBeam1];

                    Beam *newBeam2 = [[Beam alloc] init];
                    newBeam2.x = beam.x;
                    newBeam2.y = beam.y;
                    newBeam2.direction = DirectionRight;
                    beam = newBeam2;
                } else {
                    switch (beam.direction) {
                        case DirectionLeft: beam.y--; break;
                        case DirectionRight: beam.y++; break;
                        default: break;
                    }
                }
                break;
        }

        if (beam.x >= 0 && beam.x < grid.tiles.count && beam.y >= 0 && beam.y < grid.tiles[0].count) {
            [beams addObject:beam];
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        Grid *grid = [[Grid alloc] initWithFile:filePath];
        simulateBeam(grid);
        NSInteger energizedCount = [grid countEnergizedTiles];
        NSLog(@"Number of energized tiles: %ld", energizedCount);
    }
    return 0;
}