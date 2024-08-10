#import <Foundation/Foundation.h>

@interface Reindeer : NSObject
@property (nonatomic) NSInteger speed;
@property (nonatomic) NSInteger flyTime;
@property (nonatomic) NSInteger restTime;
@property (nonatomic) NSInteger distance;
@property (nonatomic) NSInteger points;
@property (nonatomic) BOOL flying;
@property (nonatomic) NSInteger timeInMode;
@end

@implementation Reindeer
@end

NSArray<Reindeer *> *readReindeerDetails(NSString *filename) {
    NSMutableArray<Reindeer *> *reindeers = [NSMutableArray array];
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString *> *lines = [content componentsSeparatedByString:@"\n"];
    
    for (NSString *line in lines) {
        if (line.length == 0) continue;
        NSArray<NSString *> *parts = [line componentsSeparatedByString:@" "];
        Reindeer *reindeer = [[Reindeer alloc] init];
        reindeer.speed = [parts[3] integerValue];
        reindeer.flyTime = [parts[6] integerValue];
        reindeer.restTime = [parts[13] integerValue];
        reindeer.flying = YES;
        [reindeers addObject:reindeer];
    }
    return reindeers;
}

void simulateRaceWithPoints(NSMutableArray<Reindeer *> *reindeers, NSInteger totalSeconds) {
    for (NSInteger i = 0; i < totalSeconds; i++) {
        NSInteger maxDistance = 0;
        for (Reindeer *reindeer in reindeers) {
            if (reindeer.flying) reindeer.distance += reindeer.speed;
            reindeer.timeInMode++;
            if ((reindeer.flying && reindeer.timeInMode == reindeer.flyTime) || 
                (!reindeer.flying && reindeer.timeInMode == reindeer.restTime)) {
                reindeer.flying = !reindeer.flying;
                reindeer.timeInMode = 0;
            }
            maxDistance = MAX(maxDistance, reindeer.distance);
        }
        for (Reindeer *reindeer in reindeers) {
            if (reindeer.distance == maxDistance) reindeer.points++;
        }
    }
}

NSInteger findMaxPoints(NSArray<Reindeer *> *reindeers) {
    NSInteger maxPoints = 0;
    for (Reindeer *reindeer in reindeers) {
        maxPoints = MAX(maxPoints, reindeer.points);
    }
    return maxPoints;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<Reindeer *> *reindeers = readReindeerDetails(@"input.txt");
        NSMutableArray<Reindeer *> *mutableReindeers = [reindeers mutableCopy];
        simulateRaceWithPoints(mutableReindeers, 2503);
        NSInteger maxPoints = findMaxPoints(mutableReindeers);
        NSLog(@"%ld", (long)maxPoints);
    }
    return 0;
}