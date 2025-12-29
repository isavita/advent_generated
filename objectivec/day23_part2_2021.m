
#import <Foundation/Foundation.h>

/**
 * State represents the burrow configuration including the hallway and side rooms.
 * It is used as a node in the A* search graph.
 */
@interface State : NSObject <NSCopying>
@property (nonatomic, copy) NSString *hallway;
@property (nonatomic, copy) NSArray<NSString *> *rooms;
@property (nonatomic, assign) int cost;
@property (nonatomic, assign) int heuristic;
- (NSString *)key;
@end

@implementation State
- (NSString *)key {
    return [NSString stringWithFormat:@"%@%@%@%@%@", _hallway, _rooms[0], _rooms[1], _rooms[2], _rooms[3]];
}
- (id)copyWithZone:(NSZone *)zone {
    State *copy = [[State alloc] init];
    copy.hallway = self.hallway;
    copy.rooms = self.rooms;
    copy.cost = self.cost;
    copy.heuristic = self.heuristic;
    return copy;
}
- (BOOL)isEqual:(id)object {
    if (![object isKindOfClass:[State class]]) return NO;
    return [[self key] isEqualToString:[object key]];
}
- (NSUInteger)hash {
    return [[self key] hash];
}
@end

/**
 * MinHeap implementation for use as a priority queue in the A* algorithm.
 */
@interface MinHeap : NSObject
@property (nonatomic, strong) NSMutableArray<State *> *heap;
- (void)push:(State *)state;
- (State *)pop;
- (NSUInteger)count;
@end

@implementation MinHeap
- (instancetype)init {
    self = [super init];
    if (self) { _heap = [NSMutableArray array]; }
    return self;
}
- (NSUInteger)count { return _heap.count; }
- (void)push:(State *)state {
    [_heap addObject:state];
    NSUInteger idx = _heap.count - 1;
    while (idx > 0) {
        NSUInteger parent = (idx - 1) / 2;
        if (_heap[idx].cost + _heap[idx].heuristic < _heap[parent].cost + _heap[parent].heuristic) {
            [_heap exchangeObjectAtIndex:idx withObjectAtIndex:parent];
            idx = parent;
        } else break;
    }
}
- (State *)pop {
    if (_heap.count == 0) return nil;
    State *top = _heap[0];
    State *last = _heap.lastObject;
    [_heap removeLastObject];
    if (_heap.count > 0) {
        _heap[0] = last;
        NSUInteger idx = 0;
        while (YES) {
            NSUInteger left = 2 * idx + 1;
            NSUInteger right = 2 * idx + 2;
            NSUInteger smallest = idx;
            if (left < _heap.count && (_heap[left].cost + _heap[left].heuristic < _heap[smallest].cost + _heap[smallest].heuristic))
                smallest = left;
            if (right < _heap.count && (_heap[right].cost + _heap[right].heuristic < _heap[smallest].cost + _heap[smallest].heuristic))
                smallest = right;
            if (smallest != idx) {
                [_heap exchangeObjectAtIndex:idx withObjectAtIndex:smallest];
                idx = smallest;
            } else break;
        }
    }
    return top;
}
@end

/**
 * Helper methods for game logic.
 */
int getEnergy(char amph) {
    if (amph == 'A') return 1;
    if (amph == 'B') return 10;
    if (amph == 'C') return 100;
    if (amph == 'D') return 1000;
    return 0;
}

int getRoomX(int roomIdx) {
    return 2 + 2 * roomIdx;
}

// Minimal energy to reach goal (must be admissible)
int calculateHeuristic(NSString *hallway, NSArray *rooms, int roomSize) {
    int h = 0;
    for (int i = 0; i < 11; i++) {
        char c = [hallway characterAtIndex:i];
        if (c >= 'A' && c <= 'D') {
            int type = c - 'A';
            h += (abs(i - getRoomX(type)) + 1) * getEnergy(c);
        }
    }
    for (int r = 0; r < 4; r++) {
        BOOL roomWrong = NO;
        for (int d = 0; d < roomSize; d++) {
            char c = [rooms[r] characterAtIndex:d];
            if (c != '.' && c != (char)('A' + r)) { roomWrong = YES; break; }
        }
        for (int d = 0; d < roomSize; d++) {
            char c = [rooms[r] characterAtIndex:d];
            if (c == '.') continue;
            int type = c - 'A';
            if (type != r) {
                h += (d + 1 + abs(getRoomX(r) - getRoomX(type)) + 1) * getEnergy(c);
            } else if (roomWrong) {
                h += (d + 1 + 2) * getEnergy(c);
            }
        }
    }
    return h;
}

BOOL isSolved(NSArray *rooms, int roomSize) {
    for (int i = 0; i < 4; i++) {
        char target = 'A' + i;
        for (int j = 0; j < roomSize; j++) {
            if ([rooms[i] characterAtIndex:j] != target) return NO;
        }
    }
    return YES;
}

BOOL isHallwayClear(int start, int end, NSString *hallway) {
    int low = MIN(start, end);
    int high = MAX(start, end);
    for (int i = low; i <= high; i++) {
        if (i == start) continue;
        if ([hallway characterAtIndex:i] != '.') return NO;
    }
    return YES;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;

        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray *cleanLines = [NSMutableArray array];
        for (NSString *line in lines) {
            if ([line containsString:@"A"] || [line containsString:@"B"] || [line containsString:@"C"] || [line containsString:@"D"]) {
                [cleanLines addObject:line];
            }
        }
        if (cleanLines.count < 2) return 1;

        // Part 2: Inserting folded extra information
        int roomSize = 4;
        NSMutableArray *rooms = [NSMutableArray arrayWithCapacity:4];
        for (int i = 0; i < 4; i++) {
            int colIdxs[] = {3, 5, 7, 9};
            int colIdx = colIdxs[i];
            NSString *r = [NSString stringWithFormat:@"%C%C%C%C",
                           [cleanLines[0] characterAtIndex:colIdx],
                           [@"DCBA" characterAtIndex:i],
                           [@"DBAC" characterAtIndex:i],
                           [cleanLines[1] characterAtIndex:colIdx]];
            [rooms addObject:r];
        }

        State *initialState = [State new];
        initialState.hallway = @"...........";
        initialState.rooms = rooms;
        initialState.cost = 0;
        initialState.heuristic = calculateHeuristic(initialState.hallway, initialState.rooms, roomSize);

        MinHeap *pq = [MinHeap new];
        [pq push:initialState];

        NSMutableDictionary *visited = [NSMutableDictionary new];
        visited[[initialState key]] = @0;

        while (pq.count > 0) {
            State *current = [pq pop];
            NSString *currKey = [current key];
            if (current.cost > [visited[currKey] intValue]) continue;
            if (isSolved(current.rooms, roomSize)) {
                printf("%d\n", current.cost);
                return 0;
            }

            // Move generation logic: From Hallway to Room
            for (int i = 0; i < 11; i++) {
                char amph = [current.hallway characterAtIndex:i];
                if (amph < 'A' || amph > 'D') continue;
                int rIdx = amph - 'A';
                int targetX = getRoomX(rIdx);
                if (isHallwayClear(i, targetX, current.hallway)) {
                    int targetDepth = -1;
                    BOOL roomValid = YES;
                    for (int d = 0; d < roomSize; d++) {
                        char c = [current.rooms[rIdx] characterAtIndex:d];
                        if (c == '.') targetDepth = d;
                        else if (c != amph) { roomValid = NO; break; }
                    }
                    if (roomValid && targetDepth != -1) {
                        int moveCost = (abs(i - targetX) + targetDepth + 1) * getEnergy(amph);
                        NSMutableArray *newRooms = [current.rooms mutableCopy];
                        NSMutableString *ns = [newRooms[rIdx] mutableCopy];
                        [ns replaceCharactersInRange:NSMakeRange(targetDepth, 1) withString:[NSString stringWithFormat:@"%c", amph]];
                        newRooms[rIdx] = ns;
                        State *next = [State new];
                        next.hallway = [current.hallway stringByReplacingCharactersInRange:NSMakeRange(i, 1) withString:@"."];
                        next.rooms = newRooms;
                        next.cost = current.cost + moveCost;
                        next.heuristic = calculateHeuristic(next.hallway, next.rooms, roomSize);
                        NSString *nk = [next key];
                        if (!visited[nk] || [visited[nk] intValue] > next.cost) {
                            visited[nk] = @(next.cost); [pq push:next];
                        }
                    }
                }
            }

            // Move generation logic: From Room to Hallway
            for (int rIdx = 0; rIdx < 4; rIdx++) {
                int topDepth = -1; char amph = '.';
                for (int d = 0; d < roomSize; d++) {
                    if ([current.rooms[rIdx] characterAtIndex:d] != '.') {
                        topDepth = d; amph = [current.rooms[rIdx] characterAtIndex:d]; break;
                    }
                }
                if (topDepth == -1) continue;
                BOOL shouldMove = NO;
                for (int d = topDepth; d < roomSize; d++) {
                    if ([current.rooms[rIdx] characterAtIndex:d] != (char)('A' + rIdx)) {
                        shouldMove = YES; break;
                    }
                }
                if (!shouldMove) continue;
                int startX = getRoomX(rIdx);
                int allowed[] = {0, 1, 3, 5, 7, 9, 10};
                for (int j = 0; j < 7; j++) {
                    int hIdx = allowed[j];
                    if (isHallwayClear(startX, hIdx, current.hallway)) {
                        int moveCost = (topDepth + 1 + abs(startX - hIdx)) * getEnergy(amph);
                        NSMutableArray *newRooms = [current.rooms mutableCopy];
                        NSMutableString *ns = [newRooms[rIdx] mutableCopy];
                        [ns replaceCharactersInRange:NSMakeRange(topDepth, 1) withString:@"."];
                        newRooms[rIdx] = ns;
                        State *next = [State new];
                        next.hallway = [current.hallway stringByReplacingCharactersInRange:NSMakeRange(hIdx, 1) withString:[NSString stringWithFormat:@"%c", amph]];
                        next.rooms = newRooms;
                        next.cost = current.cost + moveCost;
                        next.heuristic = calculateHeuristic(next.hallway, next.rooms, roomSize);
                        NSString *nk = [next key];
                        if (!visited[nk] || [visited[nk] intValue] > next.cost) {
                            visited[nk] = @(next.cost); [pq push:next];
                        }
                    }
                }
            }
        }
    }
    return 0;
}
