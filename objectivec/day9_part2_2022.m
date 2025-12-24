#import <Foundation/Foundation.h>

typedef struct { int x, y; } Pt;

static inline Pt mv(Pt p, char d){
    switch(d){
        case 'U': p.y++; break;
        case 'R': p.x++; break;
        case 'D': p.y--; break;
        case 'L': p.x--; break;
    }
    return p;
}

static inline Pt fl(Pt h, Pt t){
    int dx = h.x - t.x, dy = h.y - t.y;
    if (abs(dx)<=1 && abs(dy)<=1) return t;
    t.x += dx ? dx/abs(dx) : 0;
    t.y += dy ? dy/abs(dy) : 0;
    return t;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        NSArray *lines = [txt componentsSeparatedByCharactersInSet:
                         [NSCharacterSet newlineCharacterSet]];
        Pt rope[10] = {0};
        NSMutableSet *vis = [NSMutableSet set];
        [vis addObject:[NSValue valueWithBytes:&rope[9] objCType:@encode(Pt)]];
        for (NSString *l in lines) {
            if (l.length<3) continue;
            char dir = [l characterAtIndex:0];
            int steps = [[l substringFromIndex:2] intValue];
            while (steps--) {
                rope[0] = mv(rope[0], dir);
                for (int j=1;j<10;j++) rope[j] = fl(rope[j-1], rope[j]);
                Pt tail = rope[9];
                [vis addObject:[NSValue valueWithBytes:&tail objCType:@encode(Pt)]];
            }
        }
        printf("%lu\n", [vis count]);
    }
    return 0;
}