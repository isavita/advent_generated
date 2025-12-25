
#import <Foundation/Foundation.h>

typedef struct { int x, y; } Pt;
typedef BOOL (^Pass)(void);

static inline Pt mv(Pt p, char d) {
    switch (d) {
        case 'N': return (Pt){p.x,   p.y-1};
        case 'S': return (Pt){p.x,   p.y+1};
        case 'E': return (Pt){p.x+1, p.y};
        case 'W': return (Pt){p.x-1, p.y};
        default:  return p;
    }
}

@interface NSValue (Pt)
+ (instancetype)valueWithPt:(Pt)p;
- (Pt)ptValue;
@end
@implementation NSValue (Pt)
+ (instancetype)valueWithPt:(Pt)p { return [self valueWithBytes:&p objCType:@encode(Pt)]; }
- (Pt)ptValue { Pt p; [self getValue:&p]; return p; }
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *e;
        NSString *s = [NSString stringWithContentsOfFile:@"input.txt"
                                               encoding:NSUTF8StringEncoding
                                                  error:&e];
        if (!s) { NSLog(@"%@", e); return 1; }
        NSString *r = [s substringWithRange:NSMakeRange(1, s.length-2)];

        NSMutableDictionary *g = [NSMutableDictionary dictionary];
        NSMutableArray *stk = [NSMutableArray array];
        Pt cur = {0,0};

        for (NSUInteger i = 0; i < r.length; ++i) {
            unichar c = [r characterAtIndex:i];
            if (c == '(')           [stk addObject:[NSValue valueWithPt:cur]];
            else if (c == '|')      cur = [stk.lastObject ptValue];
            else if (c == ')')      cur = [stk.lastObject ptValue], [stk removeLastObject];
            else {
                Pt n = mv(cur, (char)c);
                NSMutableDictionary *row = g[[NSValue valueWithPt:cur]] ?: [NSMutableDictionary dictionary];
                row[[NSValue valueWithPt:n]] = @YES;
                g[[NSValue valueWithPt:cur]] = row;
                cur = n;
            }
        }

        NSMutableDictionary *vis = [NSMutableDictionary dictionary];
        NSMutableArray *q = [NSMutableArray arrayWithObject:[NSValue valueWithPt:(Pt){0,0}]];
        int maxDoors = 0;

        while (q.count) {
            NSValue *kv = q.firstObject;
            [q removeObjectAtIndex:0];
            Pt p = [kv ptValue];
            int d = [vis[kv] intValue];
            NSMutableDictionary *row = g[kv];
            for (NSValue *nk in row) {
                if (!vis[nk]) {
                    int nd = d + 1;
                    vis[nk] = @(nd);
                    if (nd > maxDoors) maxDoors = nd;
                    [q addObject:nk];
                }
            }
        }
        printf("%d\n", maxDoors);
    }
    return 0;
}
