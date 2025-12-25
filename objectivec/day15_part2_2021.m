
#import <Foundation/Foundation.h>

typedef struct { int x, y, risk; } Pos;

@interface PriorityQueue : NSObject
- (void)push:(Pos)p;
- (Pos)pop;
- (BOOL)isEmpty;
@end

@implementation PriorityQueue {
    NSMutableArray<NSValue *> *heap;
}
- (instancetype)init {
    self = [super init];
    heap = [NSMutableArray array];
    return self;
}
- (void)push:(Pos)p {
    [heap addObject:[NSValue valueWithBytes:&p objCType:@encode(Pos)]];
    NSUInteger c = heap.count - 1;
    while (c) {
        NSUInteger p = (c - 1) / 2;
        Pos child, parent;
        [heap[c] getValue:&child];
        [heap[p] getValue:&parent];
        if (child.risk >= parent.risk) break;
        [heap exchangeObjectAtIndex:c withObjectAtIndex:p];
        c = p;
    }
}
- (Pos)pop {
    Pos top; [heap[0] getValue:&top];
    NSUInteger last = heap.count - 1;
    [heap exchangeObjectAtIndex:0 withObjectAtIndex:last];
    [heap removeLastObject];
    NSUInteger n = heap.count, i = 0;
    while (i < n) {
        NSUInteger sm = i, l = 2*i+1, r = 2*i+2;
        Pos pi, pl, pr;
        [heap[i] getValue:&pi];
        if (l < n) {
            [heap[l] getValue:&pl];
            if (pl.risk < pi.risk) sm = l;
        }
        if (r < n) {
            [heap[r] getValue:&pr];
            [heap[sm] getValue:&pi];
            if (pr.risk < pi.risk) sm = r;
        }
        if (sm == i) break;
        [heap exchangeObjectAtIndex:i withObjectAtIndex:sm];
        i = sm;
    }
    return top;
}
- (BOOL)isEmpty { return heap.count == 0; }
@end

static int dijkstra(NSMutableArray<NSMutableArray<NSNumber *> *> *grid) {
    int rows = (int)grid.count, cols = (int)grid[0].count;
    int dist[rows][cols];
    for (int i = 0; i < rows; i++)
        for (int j = 0; j < cols; j++)
            dist[i][j] = INT_MAX;
    dist[0][0] = 0;
    PriorityQueue *pq = [[PriorityQueue alloc] init];
    [pq push:(Pos){0,0,0}];
    int dx[] = {1,0,-1,0}, dy[] = {0,1,0,-1};
    while (![pq isEmpty]) {
        Pos cur = [pq pop];
        if (cur.x == rows-1 && cur.y == cols-1) return cur.risk;
        for (int d = 0; d < 4; d++) {
            int nx = cur.x + dx[d], ny = cur.y + dy[d];
            if (nx<0||ny<0||nx>=rows||ny>=cols) continue;
            int nr = cur.risk + [grid[nx][ny] intValue];
            if (nr < dist[nx][ny]) {
                dist[nx][ny] = nr;
                [pq push:(Pos){nx,ny,nr}];
            }
        }
    }
    return -1;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                               encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [txt componentsSeparatedByString:@"\n"];
        NSMutableArray<NSMutableArray<NSNumber *> *> *base = [NSMutableArray array];
        for (NSString *l in lines) {
            if (l.length == 0) continue;
            NSMutableArray<NSNumber *> *row = [NSMutableArray array];
            for (int i = 0; i < l.length; i++)
                [row addObject:@(l.UTF8String[i] - '0')];
            [base addObject:row];
        }
        int R = (int)base.count, C = (int)base[0].count;
        NSMutableArray<NSMutableArray<NSNumber *> *> *big = [NSMutableArray arrayWithCapacity:R*5];
        for (int i = 0; i < R*5; i++) {
            NSMutableArray<NSNumber *> *row = [NSMutableArray arrayWithCapacity:C*5];
            for (int j = 0; j < C*5; j++) {
                int v = [base[i%R][j%C] intValue] + i/R + j/C;
                if (v > 9) v -= 9;
                [row addObject:@(v)];
            }
            [big addObject:row];
        }
        printf("%d\n", dijkstra(big));
    }
    return 0;
}
