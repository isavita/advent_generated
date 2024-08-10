#import <Foundation/Foundation.h>

@interface Particle : NSObject
@property (nonatomic) NSArray<NSNumber *> *p;
@property (nonatomic) NSArray<NSNumber *> *v;
@property (nonatomic) NSArray<NSNumber *> *a;
@end

@implementation Particle
@end

int manhattan(NSArray<NSNumber *> *coords) {
    return abs(coords[0].intValue) + abs(coords[1].intValue) + abs(coords[2].intValue);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) return 1;

        NSMutableArray<Particle *> *particles = [NSMutableArray array];
        for (NSString *line in [input componentsSeparatedByString:@"\n"]) {
            if (line.length == 0) continue;
            NSArray *parts = [line componentsSeparatedByString:@", "];

            Particle *particle = [[Particle alloc] init];
            NSMutableArray<NSNumber *> *p = [NSMutableArray arrayWithCapacity:3];
            NSMutableArray<NSNumber *> *v = [NSMutableArray arrayWithCapacity:3];
            NSMutableArray<NSNumber *> *a = [NSMutableArray arrayWithCapacity:3];

            for (int i = 0; i < 3; i++) {
                NSString *part = parts[i];
                NSString *coordsString = [part substringWithRange:NSMakeRange(3, part.length - 4)];
                NSArray *coords = [coordsString componentsSeparatedByString:@","];

                for (NSString *coord in coords) {
                    if (i == 0) [p addObject:@(coord.intValue)];
                    else if (i == 1) [v addObject:@(coord.intValue)];
                    else if (i == 2) [a addObject:@(coord.intValue)];
                }
            }

            particle.p = p;
            particle.v = v;
            particle.a = a;
            [particles addObject:particle];
        }

        int closestParticle = -1;
        int minAccel = INT_MAX, minVelocity = INT_MAX, minPosition = INT_MAX;

        for (int i = 0; i < particles.count; i++) {
            Particle *particle = particles[i];
            int accel = manhattan(particle.a);
            int velocity = manhattan(particle.v);
            int position = manhattan(particle.p);

            if (accel < minAccel || (accel == minAccel && velocity < minVelocity) ||
                (accel == minAccel && velocity == minVelocity && position < minPosition)) {
                minAccel = accel;
                minVelocity = velocity;
                minPosition = position;
                closestParticle = i;
            }
        }

        NSLog(@"%d", closestParticle);
    }
    return 0;
}