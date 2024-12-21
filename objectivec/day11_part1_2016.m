
#import <Foundation/Foundation.h>

@interface Half : NSObject
@property (nonatomic, assign) BOOL isChip;
@property (nonatomic, strong) NSString *material;
- (instancetype)initWithIsChip:(BOOL)isChip material:(NSString *)material;
- (NSString *)description;
@end

@implementation Half
- (instancetype)initWithIsChip:(BOOL)isChip material:(NSString *)material {
    self = [super init];
    if (self) {
        _isChip = isChip;
        _material = material;
    }
    return self;
}
- (NSString *)description {
    return [NSString stringWithFormat:@"%@ %@", _material, _isChip ? @"microchip" : @"generator"];
}
@end

@interface State : NSObject
@property (nonatomic, strong) NSMutableArray<NSMutableArray<Half *> *> *floors;
@property (nonatomic, assign) NSInteger elevatorLevel;
@property (nonatomic, assign) NSInteger steps;
- (instancetype)initWithInput:(NSString *)input;
- (NSString *)description;
- (NSString *)hashKey;
- (BOOL)isValid;
- (BOOL)isDone;
- (NSArray<State *> *)getNextStates;
- (State *)clone;
- (NSArray<NSArray<NSNumber *> *> *)getMovablePermIndices;
@end

@implementation State
- (instancetype)initWithInput:(NSString *)input {
    self = [super init];
    if (self) {
        _floors = [NSMutableArray arrayWithCapacity:4];
        for (int i = 0; i < 4; i++) {
            [_floors addObject:[NSMutableArray array]];
        }
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        for (NSInteger lineIndex = 0; lineIndex < lines.count; lineIndex++) {
            NSString *line = lines[lineIndex];
            NSArray *parts = [line componentsSeparatedByString:@" "];
            NSMutableArray *trimmedParts = [NSMutableArray arrayWithCapacity:parts.count];
            for (NSString *part in parts) {
                [trimmedParts addObject:[part stringByTrimmingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@",."]]];
            }
            for (NSInteger i = 0; i < trimmedParts.count; i++) {
                NSString *word = trimmedParts[i];
                if ([word isEqualToString:@"generator"]) {
                    NSString *material = trimmedParts[i - 1];
                    [_floors[lineIndex] addObject:[[Half alloc] initWithIsChip:NO material:material]];
                } else if ([word isEqualToString:@"microchip"]) {
                    NSString *material = [trimmedParts[i - 1] substringToIndex:[trimmedParts[i - 1] rangeOfString:@"-comp"].location];
                    [_floors[lineIndex] addObject:[[Half alloc] initWithIsChip:YES material:material]];
                }
            }
        }
        _elevatorLevel = 0;
        _steps = 0;
    }
    return self;
}

- (NSString *)description {
    NSMutableString *sb = [NSMutableString string];
    [sb appendFormat:@"Level %ld x Steps %ld\n", (long)_elevatorLevel, (long)_steps];
    for (NSInteger i = 0; i < _floors.count; i++) {
        [sb appendFormat:@"  %ld: %@\n", (long)i, _floors[i]];
    }
    return sb;
}

- (NSString *)hashKey {
    NSMutableDictionary<NSString *, NSNumber *> *mapGenToIndex = [NSMutableDictionary dictionary];
    NSMutableDictionary<NSString *, NSNumber *> *mapChipToIndex = [NSMutableDictionary dictionary];
    for (NSInteger flIndex = 0; flIndex < _floors.count; flIndex++) {
        NSArray<Half *> *fl = _floors[flIndex];
        for (Half *half in fl) {
            if (half.isChip) {
                mapChipToIndex[half.material] = @(flIndex);
            } else {
                mapGenToIndex[half.material] = @(flIndex);
            }
        }
    }
    NSMutableArray<NSArray<NSNumber *> *> *genChipPairs = [NSMutableArray array];
    for (NSString *material in mapGenToIndex) {
        [genChipPairs addObject:@[mapGenToIndex[material], mapChipToIndex[material]]];
    }
    [genChipPairs sortUsingComparator:^NSComparisonResult(NSArray<NSNumber *> *obj1, NSArray<NSNumber *> *obj2) {
        if ([obj1[0] integerValue] != [obj2[0] integerValue]) {
            return [obj1[0] compare:obj2[0]];
        }
        return [obj1[1] compare:obj2[1]];
    }];
    return [NSString stringWithFormat:@"%ld%@", (long)_elevatorLevel, genChipPairs];
}

- (BOOL)isValid {
    for (NSArray<Half *> *floor in _floors) {
        NSMutableSet<NSString *> *gensSeen = [NSMutableSet set];
        for (Half *half in floor) {
            if (!half.isChip) {
                [gensSeen addObject:half.material];
            }
        }
        if (gensSeen.count == 0) {
            continue;
        }
        for (Half *half in floor) {
            if (half.isChip && ![gensSeen containsObject:half.material]) {
                return NO;
            }
        }
    }
    return YES;
}

- (BOOL)isDone {
    NSInteger lenSum = 0;
    for (NSInteger i = 0; i < 3; i++) {
        lenSum += _floors[i].count;
    }
    return lenSum == 0;
}

- (NSArray<NSArray<NSNumber *> *> *)getMovablePermIndices {
    NSMutableArray<NSMutableArray<NSNumber *> *> *permsToMove = [NSMutableArray array];
    NSArray<Half *> *currentLevel = _floors[_elevatorLevel];
    for (NSInteger i = 0; i < currentLevel.count; i++) {
        for (NSInteger j = i + 1; j < currentLevel.count; j++) {
            [permsToMove addObject:[NSMutableArray arrayWithObjects:@(i), @(j), nil]];
        }
    }
    for (NSInteger i = 0; i < currentLevel.count; i++) {
        [permsToMove addObject:[NSMutableArray arrayWithObject:@(i)]];
    }
    return permsToMove;
}

- (State *)clone {
    State *cl = [[State alloc] init];
    cl.elevatorLevel = _elevatorLevel;
    cl.steps = _steps;
    cl.floors = [NSMutableArray arrayWithCapacity:4];
    for (NSArray<Half *> *floor in _floors) {
        [cl.floors addObject:[NSMutableArray arrayWithArray:floor]];
    }
    return cl;
}

- (NSArray<State *> *)getNextStates {
    NSMutableArray<State *> *futureStates = [NSMutableArray array];
    NSArray<NSArray<NSNumber *> *> *movablePermIndices = [self getMovablePermIndices];
    NSArray<NSNumber *> *eleDiffs;
    if (_elevatorLevel < _floors.count - 1) {
        eleDiffs = @[@1];
    }
    if (_elevatorLevel > 0) {
        if (eleDiffs) {
            eleDiffs = [eleDiffs arrayByAddingObject:@-1];
        } else {
            eleDiffs = @[@-1];
        }
    }
    if (!eleDiffs) {
        return futureStates;
    }
    for (NSNumber *eleDiff in eleDiffs) {
        for (NSArray<NSNumber *> *permIndices in movablePermIndices) {
            State *cl = [self clone];
            cl.elevatorLevel += [eleDiff integerValue];
            cl.steps++;
            NSInteger oldLevel = _elevatorLevel;
            NSInteger newLevel = cl.elevatorLevel;
            for (NSNumber *index in permIndices) {
                [cl.floors[newLevel] addObject:cl.floors[oldLevel][[index integerValue]]];
            }
            for (NSInteger in = permIndices.count - 1; in >= 0; in--) {
                NSInteger index = [permIndices[in] integerValue];
                cl.floors[oldLevel][index] = cl.floors[oldLevel].lastObject;
                [cl.floors[oldLevel] removeLastObject];
            }
            if ([cl isValid]) {
                [futureStates addObject:cl];
            }
        }
    }
    return futureStates;
}
@end

NSInteger rtgHellDay(NSString *input) {
    State *currentState = [[State alloc] initWithInput:input];
    NSMutableArray<State *> *queue = [NSMutableArray arrayWithObject:currentState];
    NSMutableDictionary<NSString *, NSNumber *> *prevStates = [NSMutableDictionary dictionary];
    while (queue.count > 0) {
        State *front = queue.firstObject;
        [queue removeObjectAtIndex:0];
        if ([front isDone]) {
            return front.steps;
        }
        NSString *hash = [front hashKey];
        if (prevStates[hash]) {
            continue;
        }
        prevStates[hash] = @(YES);
        NSArray<State *> *nextStates = [front getNextStates];
        [queue addObjectsFromArray:nextStates];
    }
    return -1;
}

NSString *readFile(NSString *path) {
    NSError *error;
    NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"Error reading file: %@", error);
        return nil;
    }
    return [content stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
        NSString *input = readFile(filePath);
        if (input) {
            NSInteger ans = rtgHellDay(input);
            NSLog(@"%ld", (long)ans);
        }
    }
    return 0;
}
