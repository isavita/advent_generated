
#import <Foundation/Foundation.h>

typedef struct { long long x, y; } Coord;
static inline Coord C(long long x, long long y) { return (Coord){x, y}; }
static inline Coord add(Coord a, Coord b) { return C(a.x + b.x, a.y + b.y); }
static inline Coord mul(Coord v, long long s) { return C(v.x * s, v.y * s); }
static inline long long absll(long long x) { return x < 0 ? -x : x; }

static long long hexToLL(NSString *hex) {
    return strtoll(hex.UTF8String, NULL, 16);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:nil];
        NSArray *lines = [data componentsSeparatedByCharactersInSet:
                          [NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSValue*> *verts = [NSMutableArray array];
        Coord cur = {0, 0};
        [verts addObject:[NSValue valueWithBytes:&cur objCType:@encode(Coord)]];
        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" "];
            if (parts.count < 3) continue;
            NSString *color = parts[2];
            char dirChar = [color characterAtIndex:7];
            NSString *lenHex = [color substringWithRange:NSMakeRange(2, 5)];
            long long len = hexToLL(lenHex);
            Coord dir;
            switch (dirChar) {
                case '3': dir = C( 0, -1); break;
                case '2': dir = C(-1,  0); break;
                case '1': dir = C( 0,  1); break;
                default:  dir = C( 1,  0); break;
            }
            cur = add(cur, mul(dir, len));
            [verts addObject:[NSValue valueWithBytes:&cur objCType:@encode(Coord)]];
        }
        long long area = 0, peri = 0;
        for (NSUInteger i = 0; i < verts.count; ++i) {
            Coord v0, v1;
            [verts[i] getValue:&v0];
            [verts[(i+1)%verts.count] getValue:&v1];
            area += v0.x * v1.y - v0.y * v1.x;
            peri += absll(v0.x - v1.x) + absll(v0.y - v1.y);
        }
        area = llabs(area);
        long long inside = (area - peri + 2) / 2;
        printf("%lld\n", inside + peri);
    }
    return 0;
}
