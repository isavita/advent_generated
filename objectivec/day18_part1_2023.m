#import <Foundation/Foundation.h>

@interface Coord : NSObject
@property (nonatomic) NSInteger x;
@property (nonatomic) NSInteger y;
- (instancetype)initWithX:(NSInteger)x y:(NSInteger)y;
- (Coord *)add:(Coord *)c2;
- (Coord *)multiplyByScalar:(NSInteger)s;
@end

@implementation Coord
- (instancetype)initWithX:(NSInteger)x y:(NSInteger)y {
    self = [super init];
    if (self) {
        _x = x;
        _y = y;
    }
    return self;
}
- (Coord *)add:(Coord *)c2 {
    return [[Coord alloc] initWithX:self.x + c2.x y:self.y + c2.y];
}
- (Coord *)multiplyByScalar:(NSInteger)s {
    return [[Coord alloc] initWithX:self.x * s y:self.y * s];
}
@end

NSInteger Abs(NSInteger x) {
    return x < 0 ? -x : x;
}

NSArray<Coord *> *parseInput(NSArray<NSString *> *input) {
    Coord *current = [[Coord alloc] initWithX:0 y:0];
    NSMutableArray<Coord *> *vertices = [NSMutableArray arrayWithObject:current];
    
    NSDictionary *directions = @{
        @"U": [[Coord alloc] initWithX:0 y:-1],
        @"L": [[Coord alloc] initWithX:-1 y:0],
        @"D": [[Coord alloc] initWithX:0 y:1],
        @"R": [[Coord alloc] initWithX:1 y:0]
    };
    
    for (NSString *line in input) {
        NSArray<NSString *> *parts = [line componentsSeparatedByString:@" "];
        Coord *dir = directions[parts[0]];
        NSInteger length = [parts[1] integerValue];
        current = [current add:[dir multiplyByScalar:length]];
        [vertices addObject:current];
    }
    
    return vertices;
}

NSInteger shoelace(NSArray<Coord *> *vertices) {
    NSInteger area = 0;
    NSInteger n = vertices.count;
    
    for (NSInteger i = 0; i < n; i++) {
        NSInteger next = (i + 1) % n;
        area += vertices[i].x * vertices[next].y;
        area -= vertices[i].y * vertices[next].x;
    }
    
    return Abs(area) / 2;
}

NSInteger perimeter(NSArray<Coord *> *vertices) {
    NSInteger perim = 0;
    NSInteger n = vertices.count;
    
    for (NSInteger i = 0; i < n; i++) {
        NSInteger next = (i + 1) % n;
        perim += Abs(vertices[i].x - vertices[next].x) + Abs(vertices[i].y - vertices[next].y);
    }
    
    return perim;
}

NSInteger calculatePolygonArea(NSArray<Coord *> *vertices) {
    return shoelace(vertices) + perimeter(vertices) / 2 + 1;
}

NSInteger solve(NSArray<NSString *> *input) {
    NSArray<Coord *> *vertices = parseInput(input);
    return calculatePolygonArea(vertices);
}

NSArray<NSString *> *readFile(NSString *fileName) {
    NSString *content = [NSString stringWithContentsOfFile:fileName encoding:NSUTF8StringEncoding error:nil];
    return [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<NSString *> *input = readFile(@"input.txt");
        NSLog(@"%ld", (long)solve(input));
    }
    return 0;
}