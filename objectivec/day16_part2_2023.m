
#import <Foundation/Foundation.h>

#define MAX_H 150
#define MAX_W 150
#define MAX_QUEUE_SIZE (MAX_H * MAX_W * 4)

static char grid[MAX_H][MAX_W + 1];
static bool energized[MAX_H][MAX_W];
static bool visited[MAX_H][MAX_W][4];
static int height, width;

typedef struct {int x, y, dx, dy;} State;
static State queue[MAX_QUEUE_SIZE];
static int qh, qt;

static inline int dirIdx(int dx, int dy) {
    if (dx == 1 && dy == 0) return 0;
    if (dx == -1 && dy == 0) return 1;
    if (dx == 0 && dy == 1) return 2;
    return 3;
}
static inline void enqueue(State s){ queue[qt++] = s; }
static inline State dequeue(){ return queue[qh++]; }
static inline bool empty(){ return qh == qt; }
static inline void reset() {
    memset(energized, 0, sizeof(energized));
    memset(visited, 0, sizeof(visited));
    qh = qt = 0;
}
static int simulate(int sx, int sy, int sdx, int sdy) {
    reset();
    enqueue((State){sx, sy, sdx, sdy});
    while (!empty()) {
        State cur = dequeue();
        int nx = cur.x + cur.dx, ny = cur.y + cur.dy;
        if (nx < 0 || nx >= width || ny < 0 || ny >= height) continue;
        int di = dirIdx(cur.dx, cur.dy);
        if (visited[ny][nx][di]) continue;
        visited[ny][nx][di] = true;
        energized[ny][nx] = true;
        char c = grid[ny][nx];
        if (c == '.') {
            enqueue((State){nx, ny, cur.dx, cur.dy});
        } else if (c == '/') {
            enqueue((State){nx, ny, -cur.dy, -cur.dx});
        } else if (c == '\\') {
            enqueue((State){nx, ny, cur.dy, cur.dx});
        } else if (c == '|') {
            if (cur.dx) { enqueue((State){nx, ny, 0, 1}); enqueue((State){nx, ny, 0, -1}); }
            else enqueue((State){nx, ny, cur.dx, cur.dy});
        } else if (c == '-') {
            if (cur.dy) { enqueue((State){nx, ny, 1, 0}); enqueue((State){nx, ny, -1, 0}); }
            else enqueue((State){nx, ny, cur.dx, cur.dy});
        }
    }
    int cnt = 0;
    for (int y = 0; y < height; ++y)
        for (int x = 0; x < width; ++x)
            if (energized[y][x]) ++cnt;
    return cnt;
}
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&err];
        if (!txt) return 1;
        NSArray *lines = [txt componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray *clean = [NSMutableArray array];
        for (NSString *l in lines) if (l.length) [clean addObject:l];
        height = (int)clean.count;
        if (!height) return 1;
        width = (int)[clean[0] length];
        for (int y = 0; y < height; ++y) {
            const char *cstr = [clean[y] UTF8String];
            memcpy(grid[y], cstr, width);
            grid[y][width] = 0;
        }
        int max = 0, cur;
        for (int x = 0; x < width; ++x) { cur = simulate(x, -1, 0, 1); if (cur > max) max = cur; }
        for (int x = 0; x < width; ++x) { cur = simulate(x, height, 0, -1); if (cur > max) max = cur; }
        for (int y = 0; y < height; ++y) { cur = simulate(-1, y, 1, 0); if (cur > max) max = cur; }
        for (int y = 0; y < height; ++y) { cur = simulate(width, y, -1, 0); if (cur > max) max = cur; }
        printf("%d\n", max);
    }
    return 0;
}
