#import <Foundation/Foundation.h>

typedef struct {
    BOOL isChip;
    NSString *material;
} Halves;

typedef struct {
    Halves *floors[4][10];
    int elevatorLevel;
    int steps;
} State;

@interface Program : NSObject

- (void)run;

@end

@implementation Program

- (void)run {
    NSString *input = [self readFile:@"input.txt"];
    State *initialState = [self newInitialState:input];
    initialState->floors[0][0] = (Halves){.isChip = NO, .material = @"elerium"};
    initialState->floors[0][1] = (Halves){.isChip = YES, .material = @"elerium"};
    initialState->floors[0][2] = (Halves){.isChip = NO, .material = @"dilithium"};
    initialState->floors[0][3] = (Halves){.isChip = YES, .material = @"dilithium"};

    NSMutableArray *queue = [NSMutableArray arrayWithObject:initialState];
    NSMutableSet *prevStates = [NSMutableSet set];

    while (queue.count > 0) {
        State *front = queue[0];
        [queue removeObjectAtIndex:0];

        if ([self isDone:front]) {
            NSLog(@"%d", front->steps);
            return;
        }

        NSString *hash = [self hashKey:front];
        if ([prevStates containsObject:hash]) {
            continue;
        }
        [prevStates addObject:hash];

        NSArray *nextStates = [self getNextStates:front];
        [queue addObjectsFromArray:nextStates];
    }
}

- (State *)newInitialState:(NSString *)input {
    State *s = malloc(sizeof(State));
    s->elevatorLevel = 0;
    s->steps = 0;

    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    for (int i = 0; i < lines.count; i++) {
        NSArray *parts = [lines[i] componentsSeparatedByString:@" "];
        for (int j = 0; j < parts.count; j++) {
            NSString *word = parts[j];
            if ([word isEqualToString:@"generator"]) {
                NSString *material = parts[j - 1];
                s->floors[i][s->floors[i][0].isChip] = (Halves){.isChip = NO, .material = material};
            } else if ([word isEqualToString:@"microchip"]) {
                NSString *material = [parts[j - 1] substringToIndex:[parts[j - 1] length] - 8];
                s->floors[i][s->floors[i][0].isChip] = (Halves){.isChip = YES, .material = material};
            }
        }
    }

    return s;
}

- (NSString *)hashKey:(State *)s {
    NSMutableDictionary *mapGenToIndex = [NSMutableDictionary dictionary];
    NSMutableDictionary *mapChipToIndex = [NSMutableDictionary dictionary];
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 10; j++) {
            Halves half = s->floors[i][j];
            if (half.isChip) {
                mapChipToIndex[half.material] = @(i);
            } else {
                mapGenToIndex[half.material] = @(i);
            }
        }
    }

    NSMutableArray *genChipPairs = [NSMutableArray array];
    for (NSString *material in mapGenToIndex) {
        [genChipPairs addObject:@[@(mapGenToIndex[material].intValue), @(mapChipToIndex[material].intValue)]];
    }

    [genChipPairs sortUsingComparator:^(NSArray *a, NSArray *b) {
        if (a[0].intValue != b[0].intValue) {
            return a[0].intValue < b[0].intValue;
        }
        return a[1].intValue < b[1].intValue;
    }];

    return [NSString stringWithFormat:@"%d%@", s->elevatorLevel, genChipPairs];
}

- (BOOL)isValid:(State *)s {
    for (int i = 0; i < 4; i++) {
        NSMutableSet *gensSeen = [NSMutableSet set];
        for (int j = 0; j < 10; j++) {
            Halves half = s->floors[i][j];
            if (!half.isChip) {
                [gensSeen addObject:half.material];
            }
        }

        if (gensSeen.count == 0) {
            continue;
        }

        for (int j = 0; j < 10; j++) {
            Halves half = s->floors[i][j];
            if (half.isChip && ![gensSeen containsObject:half.material]) {
                return NO;
            }
        }
    }

    return YES;
}

- (BOOL)isDone:(State *)s {
    int lenSum = 0;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 10; j++) {
            if (s->floors[i][j].material) {
                lenSum++;
            }
        }
    }
    return lenSum == 0;
}

- (NSArray *)getMovablePermIndices:(State *)s {
    NSMutableArray *permsToMove = [NSMutableArray array];
    for (int i = 0; i < 10; i++) {
        for (int j = i + 1; j < 10; j++) {
            [permsToMove addObject:@[@(i), @(j)]];
        }
        [permsToMove addObject:@[@(i)]];
    }
    return permsToMove;
}

- (State *)clone:(State *)s {
    State *cl = malloc(sizeof(State));
    cl->elevatorLevel = s->elevatorLevel;
    cl->steps = s->steps;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 10; j++) {
            cl->floors[i][j] = s->floors[i][j];
        }
    }

    return cl;
}

- (NSArray *)getNextStates:(State *)s {
    NSMutableArray *futureStates = [NSMutableArray array];
    NSArray *movablePermIndices = [self getMovablePermIndices:s];

    NSArray *eleDiffs = @[@(1), @(-1)];
    if (s->elevatorLevel < 3) {
        eleDiffs = @[eleDiffs[0]];
    }
    if (s->elevatorLevel > 0) {
        eleDiffs = @[eleDiffs[1]];
    }

    for (NSNumber *eleDiff in eleDiffs) {
        for (NSArray *permIndices in movablePermIndices) {
            State *cl = [self clone:s];
            cl->elevatorLevel += eleDiff.intValue;
            cl->steps++;

            int oldLevel = s->elevatorLevel;
            int newLevel = cl->elevatorLevel;

            for (NSNumber *index in permIndices) {
                cl->floors[newLevel][index.intValue] = s->floors[oldLevel][index.intValue];
            }

            for (int i = permIndices.count - 1; i >= 0; i--) {
                cl->floors[oldLevel][permIndices[i].intValue] = cl->floors[oldLevel][9];
                cl->floors[oldLevel][9].material = nil;
            }

            if ([self isValid:cl]) {
                [futureStates addObject:cl];
            }
        }
    }

    return futureStates;
}

- (NSString *)readFile:(NSString *)path {
    NSError *error;
    NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"%@", error);
        return nil;
    }
    return [content stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
}

@end

int main(int argc, char *argv[]) {
    @autoreleasepool {
        Program *program = [[Program alloc] init];
        [program run];
    }
    return 0;
}