#import <Foundation/Foundation.h>

@interface Particle : NSObject
@property (nonatomic) NSInteger px, py, pz;
@property (nonatomic) NSInteger vx, vy, vz;
@property (nonatomic) NSInteger ax, ay, az;
@end

@implementation Particle
@end

NSInteger manhattanDistance(Particle *p) {
    return abs(p.px) + abs(p.py) + abs(p.pz);
}

void updateParticle(Particle *p) {
    p.vx += p.ax;
    p.vy += p.ay;
    p.vz += p.az;
    p.px += p.vx;
    p.py += p.vy;
    p.pz += p.vz;
}

BOOL particlesAreColliding(Particle *p1, Particle *p2) {
    return p1.px == p2.px && p1.py == p2.py && p1.pz == p2.pz;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];

        NSMutableArray *particles = [NSMutableArray array];

        for (NSString *line in lines) {
            if ([line length] == 0) continue;

            Particle *particle = [[Particle alloc] init];
            NSArray *components = [line componentsSeparatedByString:@", "];

            if (components.count < 3) {
                NSLog(@"Invalid input line: %@", line);
                continue;
            }

            NSInteger values[9];
            for (int i = 0; i < 3; i++) {
                NSString *valueString = [[components[i] componentsSeparatedByString:@"="][1] stringByReplacingOccurrencesOfString:@">" withString:@""];
                values[i * 3 + 0] = [[[valueString componentsSeparatedByString:@","][0] stringByReplacingOccurrencesOfString:@"<" withString:@""] integerValue];
                values[i * 3 + 1] = [[[valueString componentsSeparatedByString:@","][1] stringByReplacingOccurrencesOfString:@"<" withString:@""] integerValue];
                values[i * 3 + 2] = [[[valueString componentsSeparatedByString:@","][2] stringByReplacingOccurrencesOfString:@">" withString:@""] integerValue];
            }

            particle.px = values[0];
            particle.py = values[1];
            particle.pz = values[2];
            particle.vx = values[3];
            particle.vy = values[4];
            particle.vz = values[5];
            particle.ax = values[6];
            particle.ay = values[7];
            particle.az = values[8];

            [particles addObject:particle];
        }

        // Part 1: Find the particle that stays closest to <0,0,0> in the long term
        NSInteger minDistance = INT_MAX;
        NSInteger closestParticleIndex = -1;

        for (int i = 0; i < particles.count; i++) {
            Particle *p = particles[i];
            NSInteger distance = manhattanDistance(p);
            if (distance < minDistance) {
                minDistance = distance;
                closestParticleIndex = i;
            }
        }

        NSLog(@"Particle %ld will stay closest to <0,0,0> in the long term.", (long)closestParticleIndex);

        // Part 2: Simulate and remove collisions
        NSMutableSet *collisions = [NSMutableSet set];

        for (int tick = 0; tick < 1000; tick++) {
            NSMutableSet *positions = [NSMutableSet set];
            NSMutableSet *duplicates = [NSMutableSet set];

            for (Particle *p in particles) {
                updateParticle(p);
                NSString *positionKey = [NSString stringWithFormat:@"%ld,%ld,%ld", (long)p.px, (long)p.py, (long)p.pz];

                if ([positions containsObject:positionKey]) {
                    [duplicates addObject:positionKey];
                } else {
                    [positions addObject:positionKey];
                }
            }

            for (NSString *positionKey in duplicates) {
                [collisions addObject:positionKey];
            }

            NSMutableArray *remainingParticles = [NSMutableArray array];
            for (Particle *p in particles) {
                NSString *positionKey = [NSString stringWithFormat:@"%ld,%ld,%ld", (long)p.px, (long)p.py, (long)p.pz];
                if (![collisions containsObject:positionKey]) {
                    [remainingParticles addObject:p];
                }
            }

            particles = remainingParticles;
        }

        NSLog(@"Number of particles left after all collisions are resolved: %lu", (unsigned long)particles.count);
    }
    return 0;
}