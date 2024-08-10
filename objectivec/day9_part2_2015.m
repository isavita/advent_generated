#import <Foundation/Foundation.h>

NSDictionary *readAndParseInput(NSString *filename) {
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:&error];
    if (error) return nil;

    NSMutableDictionary *distances = [NSMutableDictionary dictionary];
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
    return distances;
}

NSArray *getUniqueLocations(NSDictionary *distances) {
    NSMutableSet *locationSet = [NSMutableSet set];
    for (NSString *from in distances) {
        [locationSet addObject:from];
        for (NSString *to in distances[from]) {
            [locationSet addObject:to];
        }
    }
    return [locationSet allObjects];
}

NSInteger calculateRouteDistance(NSArray *route, NSDictionary *distances) {
    NSInteger sum = 0;
    for (NSInteger i = 0; i < route.count - 1; i++) {
        sum += [distances[route[i]][route[i + 1]] integerValue];
    }
    return sum;
}

void permute(NSMutableArray *arr, NSInteger i, NSInteger *bestDistance, NSDictionary *distances) {
    if (i == arr.count) {
        NSInteger dist = calculateRouteDistance(arr, distances);
        if (dist > *bestDistance) *bestDistance = dist;
        return;
    }
    for (NSInteger j = i; j < arr.count; j++) {
        [arr exchangeObjectAtIndex:i withObjectAtIndex:j];
        permute(arr, i + 1, bestDistance, distances);
        [arr exchangeObjectAtIndex:i withObjectAtIndex:j];
    }
}

NSInteger findLongestRoute(NSArray *locations, NSDictionary *distances) {
    NSInteger maxDistance = 0;
    permute([locations mutableCopy], 0, &maxDistance, distances);
    return maxDistance;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSDictionary *distances = readAndParseInput(@"input.txt");
        NSArray *locations = getUniqueLocations(distances);
        NSInteger maxDistance = findLongestRoute(locations, distances);
        NSLog(@"%ld", (long)maxDistance);
    }
    return 0;
}