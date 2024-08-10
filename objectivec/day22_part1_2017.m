#import <Foundation/Foundation.h>

@interface Position : NSObject <NSCopying>

@property (nonatomic, assign) NSInteger x;
@property (nonatomic, assign) NSInteger y;

- (instancetype)initWithX:(NSInteger)x andY:(NSInteger)y;

@end

@implementation Position

- (instancetype)initWithX:(NSInteger)x andY:(NSInteger)y {
    if (self = [super init]) {
        self.x = x;
        self.y = y;
    }
    return self;
}

- (instancetype)init {
    return [self initWithX:0 andY:0];
}

- (instancetype)copyWithZone:(NSZone *)zone {
    Position *positionCopy = [[Position alloc] initWithX:self.x andY:self.y];
    return positionCopy;
}

- (BOOL)isEqual:(id)object {
    if (self == object) {
        return YES;
    }

    if (![object isKindOfClass:[Position class]]) {
        return NO;
    }

    Position *position = (Position *)object;
    return (self.x == position.x && self.y == position.y);
}

- (NSUInteger)hash {
    NSUInteger hash = (NSUInteger)self.x;
    hash = hash * 31u + (NSUInteger)self.y;
    return hash;
}

@end

int main(int argc, char *argv[]) {
    NSError *error;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (!input) {
        NSLog(@"Error reading input file: %@", error);
        return 1;
    }

    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    NSMutableDictionary *grid = [NSMutableDictionary dictionary];
    NSInteger startX, startY;
    for (NSInteger y = 0; y < lines.count; y++) {
        NSString *line = lines[y];
        for (NSInteger x = 0; x < line.length; x++) {
            if ([line characterAtIndex:x] == '#') {
                Position *pos = [[Position alloc] initWithX:x andY:y];
                grid[pos] = @YES;
            }
        }
        startX = line.length / 2;
        startY = y / 2;
    }

    NSInteger dx[] = {0, 1, 0, -1};
    NSInteger dy[] = {-1, 0, 1, 0};

    Position *start = [[Position alloc] initWithX:startX andY:startY];
    grid[start] = @YES;
    NSInteger dir = 0;
    NSInteger infectedCount = 0;

    for (NSInteger i = 0; i < 10000; i++) {
        if ([grid[start] boolValue]) {
            dir = (dir + 1) % 4;
            [grid removeObjectForKey:start];
        } else {
            dir = (dir - 1 + 4) % 4;
            grid[start] = @YES;
            infectedCount++;
        }
        start.x += dx[dir];
        start.y += dy[dir];
    }

    NSLog(@"%ld", infectedCount);
    return 0;
}