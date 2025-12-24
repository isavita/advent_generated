
#import <Foundation/Foundation.h>

typedef struct { int x, y, z; } Vec3;
typedef struct { Vec3 pos, vel; } Moon;

static void applyGravity(Moon *moons, NSUInteger n, char axis) {
    for (NSUInteger i = 0; i < n; i++) {
        for (NSUInteger j = i + 1; j < n; j++) {
            if (axis == 'x') {
                if (moons[i].pos.x > moons[j].pos.x) { moons[i].vel.x--; moons[j].vel.x++; }
                else if (moons[i].pos.x < moons[j].pos.x) { moons[i].vel.x++; moons[j].vel.x--; }
            } else if (axis == 'y') {
                if (moons[i].pos.y > moons[j].pos.y) { moons[i].vel.y--; moons[j].vel.y++; }
                else if (moons[i].pos.y < moons[j].pos.y) { moons[i].vel.y++; moons[j].vel.y--; }
            } else {
                if (moons[i].pos.z > moons[j].pos.z) { moons[i].vel.z--; moons[j].vel.z++; }
                else if (moons[i].pos.z < moons[j].pos.z) { moons[i].vel.z++; moons[j].vel.z--; }
            }
        }
    }
}
static void applyVelocity(Moon *moons, NSUInteger n, char axis) {
    for (NSUInteger i = 0; i < n; i++) {
        if (axis == 'x') moons[i].pos.x += moons[i].vel.x;
        else if (axis == 'y') moons[i].pos.y += moons[i].vel.y;
        else moons[i].pos.z += moons[i].vel.z;
    }
}
static NSUInteger findCycle(Moon *moons, Moon *initial, NSUInteger n, char axis) {
    for (NSUInteger steps = 1;; steps++) {
        applyGravity(moons, n, axis);
        applyVelocity(moons, n, axis);
        BOOL match = YES;
        for (NSUInteger i = 0; i < n; i++) {
            if ((axis == 'x' && (moons[i].pos.x != initial[i].pos.x || moons[i].vel.x != initial[i].vel.x)) ||
                (axis == 'y' && (moons[i].pos.y != initial[i].pos.y || moons[i].vel.y != initial[i].vel.y)) ||
                (axis == 'z' && (moons[i].pos.z != initial[i].pos.z || moons[i].vel.z != initial[i].vel.z))) {
                match = NO; break;
            }
        }
        if (match) return steps;
    }
}
static int64_t gcd(int64_t a, int64_t b) {
    while (b) { int64_t t = b; b = a % b; a = t; }
    return a;
}
static int64_t lcm(int64_t a, int64_t b) { return a / gcd(a, b) * b; }

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        Moon moons[32], init[32];
        NSUInteger n = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            int x, y, z;
            NSScanner *sc = [NSScanner scannerWithString:line];
            [sc scanString:@"<x=" intoString:NULL];
            [sc scanInt:&x];
            [sc scanString:@", y=" intoString:NULL];
            [sc scanInt:&y];
            [sc scanString:@", z=" intoString:NULL];
            [sc scanInt:&z];
            moons[n].pos = (Vec3){x, y, z};
            moons[n].vel = (Vec3){0,0,0};
            init[n] = moons[n];
            n++;
        }
        NSUInteger cx = findCycle(moons, init, n, 'x');
        for (NSUInteger i = 0; i < n; i++) moons[i] = init[i];
        NSUInteger cy = findCycle(moons, init, n, 'y');
        for (NSUInteger i = 0; i < n; i++) moons[i] = init[i];
        NSUInteger cz = findCycle(moons, init, n, 'z');
        int64_t result = lcm(lcm((int64_t)cx, (int64_t)cy), (int64_t)cz);
        printf("%lld\n", result);
    }
    return 0;
}
