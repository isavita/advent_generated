#import <Foundation/Foundation.h>

@interface Vec3 : NSObject
@property int x, y, z;
@end

@implementation Vec3
@end

@interface Moon : NSObject
@property Vec3 *pos, *vel;
@end

@implementation Moon
@end

int Abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

void applyGravity(NSMutableArray<Moon *> *moons) {
    for (int i = 0; i < moons.count; i++) {
        for (int j = i + 1; j < moons.count; j++) {
            Moon *moon1 = moons[i];
            Moon *moon2 = moons[j];
            if (moon1.pos.x > moon2.pos.x) {
                moon1.vel.x--;
                moon2.vel.x++;
            } else if (moon1.pos.x < moon2.pos.x) {
                moon1.vel.x++;
                moon2.vel.x--;
            }

            if (moon1.pos.y > moon2.pos.y) {
                moon1.vel.y--;
                moon2.vel.y++;
            } else if (moon1.pos.y < moon2.pos.y) {
                moon1.vel.y++;
                moon2.vel.y--;
            }

            if (moon1.pos.z > moon2.pos.z) {
                moon1.vel.z--;
                moon2.vel.z++;
            } else if (moon1.pos.z < moon2.pos.z) {
                moon1.vel.z++;
                moon2.vel.z--;
            }
        }
    }
}

void applyVelocity(NSMutableArray<Moon *> *moons) {
    for (Moon *moon in moons) {
        moon.pos.x += moon.vel.x;
        moon.pos.y += moon.vel.y;
        moon.pos.z += moon.vel.z;
    }
}

int totalEnergy(NSMutableArray<Moon *> *moons) {
    int total = 0;
    for (Moon *m in moons) {
        int pot = Abs(m.pos.x) + Abs(m.pos.y) + Abs(m.pos.z);
        int kin = Abs(m.vel.x) + Abs(m.vel.y) + Abs(m.vel.z);
        total += pot * kin;
    }
    return total;
}

int main() {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];

        NSMutableArray<Moon *> *moons = [NSMutableArray array];
        for (NSString *line in lines) {
            int x, y, z;
            sscanf([line UTF8String], "<x=%d, y=%d, z=%d>", &x, &y, &z);
            Vec3 *pos = [[Vec3 alloc] init];
            pos.x = x;
            pos.y = y;
            pos.z = z;
            Vec3 *vel = [[Vec3 alloc] init];
            vel.x = 0;
            vel.y = 0;
            vel.z = 0;
            Moon *moon = [[Moon alloc] init];
            moon.pos = pos;
            moon.vel = vel;
            [moons addObject:moon];
        }

        for (int step = 0; step < 1000; step++) {
            applyGravity(moons);
            applyVelocity(moons);
        }

        printf("%d\n", totalEnergy(moons));
    }
    return 0;
}