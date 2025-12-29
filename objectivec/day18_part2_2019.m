
#import <Foundation/Foundation.h>

typedef struct { int dist, mask; } Edge;

@interface State : NSObject
@property (nonatomic) int cost, mask;
@property (nonatomic, copy) NSArray<NSNumber *> *pos;
@end
@implementation State
@end

@interface PriorityQueue : NSObject
@property (nonatomic, strong) NSMutableArray<State *> *heap;
@end
@implementation PriorityQueue
- (instancetype)init {
    self = [super init];
    if (self) _heap = [NSMutableArray array];
    return self;
}
- (void)push:(State *)s {
    [_heap addObject:s];
    NSInteger i = _heap.count - 1;
    while (i > 0) {
        NSInteger p = (i - 1) / 2;
        if ([_heap[i] cost] >= [_heap[p] cost]) break;
        [_heap exchangeObjectAtIndex:i withObjectAtIndex:p];
        i = p;
    }
}
- (State *)pop {
    State *min = _heap[0];
    _heap[0] = _heap.lastObject;
    [_heap removeLastObject];
    NSInteger i = 0;
    while (i * 2 + 1 < _heap.count) {
        NSInteger j = i * 2 + 1;
        if (j + 1 < _heap.count && [_heap[j+1] cost] < [_heap[j] cost]) j++;
        if ([_heap[i] cost] <= [_heap[j] cost]) break;
        [_heap exchangeObjectAtIndex:i withObjectAtIndex:j];
        i = j;
    }
    return min;
}
- (BOOL)isEmpty { return _heap.count == 0; }
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 0;
        NSMutableArray<NSMutableArray<NSNumber *> *> *map = [NSMutableArray array];
        for (NSString *line in [content componentsSeparatedByString:@"\n"]) {
            if (line.length == 0) continue;
            NSMutableArray *row = [NSMutableArray array];
            for (int i = 0; i < line.length; i++) [row addObject:@([line characterAtIndex:i])];
            [map addObject:row];
        }

        BOOL modified = NO;
        for (int y = 1; y < map.count - 1 && !modified; y++) {
            for (int x = 1; x < map[0].count - 1; x++) {
                if ([map[y][x] intValue] == '@' && [map[y-1][x] intValue] == '.' && [map[y+1][x] intValue] == '.' && [map[y][x-1] intValue] == '.' && [map[y][x+1] intValue] == '.') {
                    int dy[] = {-1,-1,-1,0,0,0,1,1,1}, dx[] = {-1,0,1,-1,0,1,-1,0,1};
                    char repl[] = {'@','#','@','#','#','#','@','#','@'};
                    for (int i = 0; i < 9; i++) map[y+dy[i]][x+dx[i]] = @(repl[i]);
                    modified = YES; break;
                }
            }
        }

        NSMutableArray *robotPos = [NSMutableArray array];
        NSMutableDictionary *keyCoords = [NSMutableDictionary dictionary];
        int totalKeys = 0;
        for (int y = 0; y < map.count; y++) {
            for (int x = 0; x < map[y].count; x++) {
                char c = [map[y][x] intValue];
                if (c == '@') [robotPos addObject:@[@(x), @(y)]];
                else if (c >= 'a' && c <= 'z') {
                    keyCoords[@(c)] = @[@(x), @(y)];
                    totalKeys |= (1 << (c - 'a'));
                }
            }
        }

        int keyCount = 0;
        for(int i=0; i<26; i++) if((totalKeys >> i) & 1) keyCount++;

        NSMutableDictionary *graph = [NSMutableDictionary dictionary];
        NSMutableArray *nodes = [NSMutableArray array];
        for (int i = 0; i < robotPos.count; i++) [nodes addObject:[NSString stringWithFormat:@"@%d", i]];
        for (int i = 0; i < 26; i++) if ((totalKeys >> i) & 1) [nodes addObject:[NSString stringWithFormat:@"%c", 'a' + i]];

        for (NSString *startNode in nodes) {
            int sx, sy;
            if ([startNode hasPrefix:@"@"]) {
                int idx = [[startNode substringFromIndex:1] intValue];
                sx = [robotPos[idx][0] intValue]; sy = [robotPos[idx][1] intValue];
            } else {
                NSArray *c = keyCoords[@([startNode characterAtIndex:0])];
                sx = [c[0] intValue]; sy = [c[1] intValue];
            }

            NSMutableDictionary *dists = [NSMutableDictionary dictionary];
            NSMutableArray *q = [NSMutableArray arrayWithObject:@[@(sx), @(sy), @(0), @(0)]];
            NSMutableSet *seen = [NSMutableSet setWithObject:[NSString stringWithFormat:@"%d,%d", sx, sy]];
            int head = 0;
            while (head < q.count) {
                NSArray *curr = q[head++];
                int x = [curr[0] intValue], y = [curr[1] intValue], d = [curr[2] intValue], m = [curr[3] intValue];
                char c = [map[y][x] intValue];
                if (c >= 'a' && c <= 'z' && ![startNode isEqualToString:[NSString stringWithFormat:@"%c", c]]) {
                    dists[@(c)] = [NSData dataWithBytes:(Edge[]){d, m} length:sizeof(Edge)];
                    m |= (1 << (c - 'a'));
                }
                int dx[] = {0,0,1,-1}, dy[] = {1,-1,0,0};
                for (int i = 0; i < 4; i++) {
                    int nx = x + dx[i], ny = y + dy[i];
                    NSString *key = [NSString stringWithFormat:@"%d,%d", nx, ny];
                    if (ny >= 0 && ny < map.count && nx >= 0 && nx < map[0].count && [map[ny][nx] intValue] != '#' && ![seen containsObject:key]) {
                        [seen addObject:key];
                        int nm = m;
                        char nc = [map[ny][nx] intValue];
                        if (nc >= 'A' && nc <= 'Z') nm |= (1 << (nc - 'A'));
                        [q addObject:@[@(nx), @(ny), @(d + 1), @(nm)]];
                    }
                }
            }
            graph[startNode] = dists;
        }

        PriorityQueue *pq = [[PriorityQueue alloc] init];
        State *initial = [[State alloc] init];
        initial.cost = 0; initial.mask = 0;
        NSMutableArray *startIds = [NSMutableArray array];
        for (int i = 0; i < robotPos.count; i++) [startIds addObject:[NSString stringWithFormat:@"@%d", i]];
        initial.pos = startIds;
        [pq push:initial];

        NSMutableDictionary *visited = [NSMutableDictionary dictionary];
        while (![pq isEmpty]) {
            State *curr = [pq pop];
            if (curr.mask == totalKeys) { printf("%d\n", curr.cost); return 0; }
            NSString *stateKey = [NSString stringWithFormat:@"%@-%d", [curr.pos componentsJoinedByString:@","], curr.mask];
            if (visited[stateKey] && [visited[stateKey] intValue] <= curr.cost) continue;
            visited[stateKey] = @(curr.cost);

            for (int i = 0; i < curr.pos.count; i++) {
                NSDictionary *targets = graph[curr.pos[i]];
                for (NSNumber *kChar in targets) {
                    char c = [kChar intValue];
                    int keyBit = 1 << (c - 'a');
                    if (!(curr.mask & keyBit)) {
                        Edge e; [targets[kChar] getBytes:&e length:sizeof(Edge)];
                        if ((e.mask & curr.mask) == e.mask) {
                            State *next = [[State alloc] init];
                            next.cost = curr.cost + e.dist;
                            next.mask = curr.mask | keyBit;
                            NSMutableArray *nextPos = [curr.pos mutableCopy];
                            nextPos[i] = [NSString stringWithFormat:@"%c", c];
                            next.pos = nextPos;
                            [pq push:next];
                        }
                    }
                }
            }
        }
    }
    return 0;
}

