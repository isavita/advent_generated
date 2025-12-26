
#import <Foundation/Foundation.h>

@interface Coord : NSObject
@property int x, y, z;
- (instancetype)initWithX:(int)x Y:(int)y Z:(int)z;
@end
@implementation Coord
- (instancetype)initWithX:(int)x Y:(int)y Z:(int)z {
    if (self = [super init]) { _x = x; _y = y; _z = z; }
    return self;
}
@end

@interface Brick : NSObject
@property (nonatomic, strong) Coord *mini;
@property (nonatomic, strong) Coord *maxi;
@property (nonatomic, strong) NSMutableArray<Brick *> *basedOn;
@property (nonatomic, strong) NSMutableArray<Brick *> *support;
- (instancetype)initWithMini:(Coord *)mini maxi:(Coord *)maxi;
@end
@implementation Brick
- (instancetype)initWithMini:(Coord *)mini maxi:(Coord *)maxi {
    if (self = [super init]) {
        _mini = mini; _maxi = maxi;
        _basedOn = [NSMutableArray array];
        _support = [NSMutableArray array];
    }
    return self;
}
@end

static NSArray<NSString *> *readInput(void) {
    NSString *path = @"input.txt";
    NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
    return [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
}

static NSMutableArray<Brick *> *parseInput(NSArray<NSString *> *lines) {
    NSMutableArray<Brick *> *bricks = [NSMutableArray array];
    for (NSString *line in lines) {
        if (line.length == 0) continue;
        NSArray *parts = [line componentsSeparatedByString:@"~"];
        NSArray *a = [[parts[0] componentsSeparatedByString:@","] valueForKey:@"intValue"];
        NSArray *b = [[parts[1] componentsSeparatedByString:@","] valueForKey:@"intValue"];
        Coord *mini = [[Coord alloc] initWithX:[a[0] intValue] Y:[a[1] intValue] Z:[a[2] intValue]];
        Coord *maxi = [[Coord alloc] initWithX:[b[0] intValue] Y:[b[1] intValue] Z:[b[2] intValue]];
        [bricks addObject:[[Brick alloc] initWithMini:mini maxi:maxi]];
    }
    return bricks;
}

static void settle(NSMutableArray<Brick *> *bricks) {
    [bricks sortUsingComparator:^NSComparisonResult(Brick *b1, Brick *b2) {
        return b1.maxi.z < b2.maxi.z ? NSOrderedAscending : (b1.maxi.z > b2.maxi.z ? NSOrderedDescending : NSOrderedSame);
    }];

    for (NSInteger i = 0; i < bricks.count; ++i) {
        Brick *brick = bricks[i];
        int supportZ = 0;
        NSMutableArray<Brick *> *based = [NSMutableArray array];
        for (NSInteger j = i - 1; j >= 0; --j) {
            Brick *other = bricks[j];
            if (MAX(brick.mini.x, other.mini.x) <= MIN(brick.maxi.x, other.maxi.x) &&
                MAX(brick.mini.y, other.mini.y) <= MIN(brick.maxi.y, other.maxi.y)) {
                if (other.maxi.z == supportZ) {
                    [based addObject:other];
                } else if (other.maxi.z > supportZ) {
                    supportZ = other.maxi.z;
                    [based removeAllObjects];
                    [based addObject:other];
                }
            }
        }
        [brick.basedOn addObjectsFromArray:based];
        for (Brick *b in based) [b.support addObject:brick];
        int deltaZ = brick.maxi.z - brick.mini.z;
        brick.mini.z = supportZ + 1;
        brick.maxi.z = brick.mini.z + deltaZ;
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<NSString *> *lines = readInput();
        NSMutableArray<Brick *> *bricks = parseInput(lines);
        settle(bricks);
        NSInteger count = 0;
        for (Brick *b in bricks) {
            BOOL ok = YES;
            for (Brick *s in b.support) {
                if (s.basedOn.count <= 1) { ok = NO; break; }
            }
            if (ok) ++count;
        }
        printf("%ld\n", (long)count);
    }
    return 0;
}
