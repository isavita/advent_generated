#import <Foundation/Foundation.h>

NSArray *readBusIDs(NSString *fileName, NSError **error) {
    NSString *content = [NSString stringWithContentsOfFile:fileName encoding:NSUTF8StringEncoding error:error];
    if (*error) return nil;
    
    NSArray *lines = [content componentsSeparatedByString:@"\n"];
    NSArray *busData = [lines[1] componentsSeparatedByString:@","];
    
    NSMutableArray *ids = [NSMutableArray array];
    NSMutableArray *offsets = [NSMutableArray array];
    
    for (NSUInteger i = 0; i < busData.count; i++) {
        NSString *bus = busData[i];
        if (![bus isEqualToString:@"x"]) {
            [ids addObject:@(bus.integerValue)];
            [offsets addObject:@(i)];
        }
    }
    return @[ids, offsets];
}

NSInteger extendedGCD(NSInteger a, NSInteger b, NSInteger *x, NSInteger *y) {
    if (a == 0) {
        *x = 0; *y = 1;
        return b;
    }
    NSInteger x1, y1;
    NSInteger gcd = extendedGCD(b % a, a, &x1, &y1);
    *x = y1 - (b / a) * x1;
    *y = x1;
    return gcd;
}

NSInteger findEarliestTimestamp(NSArray *ids, NSArray *offsets) {
    NSInteger N = 1;
    for (NSNumber *id in ids) {
        N *= id.integerValue;
    }
    
    NSInteger result = 0;
    for (NSUInteger i = 0; i < ids.count; i++) {
        NSInteger id = [ids[i] integerValue];
        NSInteger ni = N / id;
        NSInteger x, y;
        extendedGCD(ni, id, &x, &y);
        result += (-[offsets[i] integerValue] + id) % id * x * ni;
    }
    return result % N;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSArray *data = readBusIDs(@"input.txt", &error);
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        NSInteger timestamp = findEarliestTimestamp(data[0], data[1]);
        NSLog(@"%ld", (long)timestamp);
    }
    return 0;
}