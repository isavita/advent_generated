#import <Foundation/Foundation.h>

@interface Elf : NSObject
@property int x, y, nextX, nextY;
@property BOOL moving;
- (instancetype)initWithX:(int)x y:(int)y;
@end
@implementation Elf
- (instancetype)initWithX:(int)x y:(int)y { self = [super init]; if (self) { _x = x; _y = y; } return self; }
@end

static inline int hashXY(int x, int y) { return x * 10000 + y; }

static BOOL aroundAllEmpty(Elf *e, NSDictionary *map, int dirs[8][2]) {
    for (int i = 0; i < 8; i++) {
        if (map[@(hashXY(e.x + dirs[i][0], e.y + dirs[i][1]))]) return NO;
    }
    return YES;
}

static BOOL elfInDirection(Elf *e, int wannaGo, NSDictionary *map, int dirs[8][2]) {
    for (int j = -1; j <= 1; j++) {
        int idx = (wannaGo + j + 8) % 8;
        if (map[@(hashXY(e.x + dirs[idx][0], e.y + dirs[idx][1]))]) return YES;
    }
    return NO;
}

static BOOL run(NSMutableArray<Elf *> *elves, NSMutableDictionary *map, int order[4], int currDir, int dirs[8][2]) {
    NSMutableDictionary *proposes = [NSMutableDictionary dictionary];
    for (Elf *elf in elves) {
        elf.moving = NO;
        if (aroundAllEmpty(elf, map, dirs)) continue;
        for (int i = 0; i < 4; i++) {
            int dir = order[(currDir + i) % 4];
            if (elfInDirection(elf, dir, map, dirs)) continue;
            int dx = dirs[dir][0], dy = dirs[dir][1];
            int destHash = hashXY(elf.x + dx, elf.y + dy);
            proposes[@(destHash)] = @([proposes[@(destHash)] intValue] + 1);
            elf.nextX = elf.x + dx; elf.nextY = elf.y + dy; elf.moving = YES; break;
        }
    }
    BOOL moved = NO;
    for (Elf *elf in elves) {
        if (!elf.moving) continue;
        int h = hashXY(elf.nextX, elf.nextY);
        if ([proposes[@(h)] intValue] > 1) { elf.moving = NO; continue; }
        moved = YES;
        [map removeObjectForKey:@(hashXY(elf.x, elf.y))];
        elf.x = elf.nextX; elf.y = elf.nextY;
        map[@(hashXY(elf.x, elf.y))] = elf;
        elf.moving = NO;
    }
    return moved;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        int dirs[8][2] = {{-1,-1},{-1,0},{-1,1},{0,1},{1,1},{1,0},{1,-1},{0,-1}};
        int order[4] = {1,5,7,3}, currDir = 0;
        NSMutableArray<Elf *> *elves = [NSMutableArray array];
        NSMutableDictionary *map = [NSMutableDictionary dictionary];
        NSArray *lines = [txt componentsSeparatedByString:@"\n"];
        for (int row = 0; row < lines.count; row++) {
            NSString *line = lines[row];
            for (int col = 0; col < line.length; col++) {
                if ([line characterAtIndex:col] == '#') {
                    Elf *e = [[Elf alloc] initWithX:row y:col];
                    [elves addObject:e];
                    map[@(hashXY(row, col))] = e;
                }
            }
        }
        int i = 0;
        while (run(elves, map, order, currDir, dirs)) { currDir = (currDir + 1) % 4; i++; }
        printf("%d\n", i + 1);
    }
    return 0;
}