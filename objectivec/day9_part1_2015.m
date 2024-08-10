#import <Foundation/Foundation.h>

@interface RouteCalculator : NSObject
- (void)calculateShortestRoute;
@end

@implementation RouteCalculator {
    NSMutableDictionary<NSString *, NSMutableDictionary<NSString *, NSNumber *> *> *distances;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        distances = [NSMutableDictionary dictionary];
    }
    return self;
}

- (void)calculateShortestRoute {
    [self readAndParseInput:@"input.txt"];
    NSArray *locations = [self getUniqueLocations];
    NSInteger minDistance = [self findShortestRoute:locations];
    NSLog(@"%ld", (long)minDistance);
}

- (void)readAndParseInput:(NSString *)filename {
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:nil];
    NSArray *lines = [content componentsSeparatedByString:@"\n"];
    
    for (NSString *line in lines) {
        NSArray *parts = [line componentsSeparatedByString:@" "];
        if (parts.count != 5) continue;
        
        NSString *from = parts[0];
        NSString *to = parts[2];
        NSInteger distance = [parts[4] integerValue];
        
        if (!distances[from]) distances[from] = [NSMutableDictionary dictionary];
        distances[from][to] = @(distance);
        
        if (!distances[to]) distances[to] = [NSMutableDictionary dictionary];
        distances[to][from] = @(distance);
    }
}

- (NSArray *)getUniqueLocations {
    NSMutableSet *locationSet = [NSMutableSet set];
    for (NSString *from in distances) {
        [locationSet addObject:from];
        for (NSString *to in distances[from]) {
            [locationSet addObject:to];
        }
    }
    return [locationSet allObjects];
}

- (NSInteger)findShortestRoute:(NSArray *)locations {
    NSInteger minDistance = NSIntegerMax;
    [self permute:locations index:0 minDistance:&minDistance];
    return minDistance;
}

- (void)permute:(NSArray *)arr index:(NSInteger)i minDistance:(NSInteger *)minDistance {
    if (i == arr.count) {
        NSInteger dist = [self calculateRouteDistance:arr];
        if (dist < *minDistance) *minDistance = dist;
        return;
    }
    NSMutableArray *mutableArr = [arr mutableCopy];
    for (NSInteger j = i; j < arr.count; j++) {
        [mutableArr exchangeObjectAtIndex:i withObjectAtIndex:j];
        [self permute:mutableArr index:i + 1 minDistance:minDistance];
        [mutableArr exchangeObjectAtIndex:i withObjectAtIndex:j];
    }
}

- (NSInteger)calculateRouteDistance:(NSArray *)route {
    NSInteger sum = 0;
    for (NSInteger i = 0; i < route.count - 1; i++) {
        sum += distances[route[i]][route[i + 1]].integerValue;
    }
    return sum;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        RouteCalculator *calculator = [[RouteCalculator alloc] init];
        [calculator calculateShortestRoute];
    }
    return 0;
}