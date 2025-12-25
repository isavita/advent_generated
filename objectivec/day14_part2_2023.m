
#import <Foundation/Foundation.h>

typedef struct { int x; int y; } Coord;

static inline Coord add(Coord a, Coord b) {
    return (Coord){a.x + b.x, a.y + b.y};
}
static inline BOOL inBounds(Coord c, int w, int h) {
    return c.x >= 0 && c.x < w && c.y >= 0 && c.y < h;
}
static inline uint64_t keyFromCoord(Coord c, int w) {
    return (uint64_t)c.x + (uint64_t)c.y * (uint64_t)w;
}
static inline NSString *keyString(Coord c) {
    return [NSString stringWithFormat:@"%d,%d", c.x, c.y];
}

@interface Grid : NSObject
@property (nonatomic) int width, height;
@property (nonatomic, strong) NSMutableDictionary<NSString*, NSString*> *data;
- (instancetype)initWithLines:(NSArray<NSString*>*)lines;
- (void)shiftSingleRockAt:(Coord)coord dir:(Coord)dir;
- (void)shiftRocks:(Coord)dir;
- (void)cycleRocks;
- (uint64_t)gridKey;
- (int)load;
@end

@implementation Grid
- (instancetype)initWithLines:(NSArray<NSString*>*)lines {
    if (self = [super init]) {
        _height = (int)lines.count;
        _width = (int)[lines[0] length];
        _data = [NSMutableDictionary dictionary];
        for (int y = 0; y < _height; ++y) {
            NSString *row = lines[y];
            for (int x = 0; x < _width; ++x) {
                unichar ch = [row characterAtIndex:x];
                if (ch != '.') {
                    _data[keyString((Coord){x, y})] = [NSString stringWithCharacters:&ch length:1];
                }
            }
        }
    }
    return self;
}
- (void)shiftSingleRockAt:(Coord)coord dir:(Coord)dir {
    NSString *k = keyString(coord);
    NSString *v = _data[k];
    if (v && [v isEqualToString:@"O"]) {
        Coord cur = coord;
        Coord nxt = add(cur, dir);
        while (inBounds(nxt, _width, _height) && !_data[keyString(nxt)]) {
            _data[keyString(nxt)] = @"O";
            [_data removeObjectForKey:k];
            cur = nxt;
            k = keyString(cur);
            nxt = add(cur, dir);
        }
    }
}
- (void)shiftRocks:(Coord)dir {
    if ((dir.x == 0 && dir.y == -1) || (dir.x == -1 && dir.y == 0)) {
        for (int x = 0; x < _width; ++x)
            for (int y = 0; y < _height; ++y)
                [self shiftSingleRockAt:(Coord){x, y} dir:dir];
    } else {
        for (int x = _width - 1; x >= 0; --x)
            for (int y = _height - 1; y >= 0; --y)
                [self shiftSingleRockAt:(Coord){x, y} dir:dir];
    }
}
- (void)cycleRocks {
    [self shiftRocks:(Coord){0, -1}];
    [self shiftRocks:(Coord){-1, 0}];
    [self shiftRocks:(Coord){0, 1}];
    [self shiftRocks:(Coord){1, 0}];
}
- (uint64_t)gridKey {
    uint64_t key = 0;
    for (NSString *k in _data) {
        if ([_data[k] isEqualToString:@"O"]) {
            NSArray<NSString*> *parts = [k componentsSeparatedByString:@","];
            int x = parts[0].intValue;
            int y = parts[1].intValue;
            key += (uint64_t)x + (uint64_t)y * (uint64_t)_width;
        }
    }
    return key;
}
- (int)load {
    int sum = 0;
    for (NSString *k in _data) {
        if ([_data[k] isEqualToString:@"O"]) {
            int y = [[k componentsSeparatedByString:@","][1] intValue];
            sum += _height - y;
        }
    }
    return sum;
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if (!content) return 1;
        NSMutableArray<NSString*> *lines = [NSMutableArray array];
        [content enumerateLinesUsingBlock:^(NSString *line, BOOL *stop) {
            [lines addObject:line];
        }];

        Grid *grid = [[Grid alloc] initWithLines:lines];
        const int cycles = 1000000000;
        NSMutableDictionary<NSNumber*, NSNumber*> *cache = [NSMutableDictionary dictionary];

        for (int i = 0; i < cycles; ++i) {
            uint64_t gk = [grid gridKey];
            NSNumber *nk = @(gk);
            NSNumber *prev = cache[nk];
            if (prev) {
                int start = prev.intValue;
                int len = i - start;
                int remaining = (cycles - start) % len;
                for (int j = 0; j < remaining; ++j) [grid cycleRocks];
                printf("%d\n", [grid load]);
                return 0;
            }
            cache[nk] = @(i);
            [grid cycleRocks];
        }
        printf("%d\n", [grid load]);
    }
    return 0;
}
