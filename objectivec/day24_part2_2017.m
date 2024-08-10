#import <Foundation/Foundation.h>

@interface BridgeComponent : NSObject
@property (nonatomic, assign) NSInteger port1;
@property (nonatomic, assign) NSInteger port2;
+ (instancetype)componentWithPort1:(NSInteger)port1 port2:(NSInteger)port2;
@end

@implementation BridgeComponent
+ (instancetype)componentWithPort1:(NSInteger)port1 port2:(NSInteger)port2 {
    BridgeComponent *component = [[BridgeComponent alloc] init];
    component.port1 = port1;
    component.port2 = port2;
    return component;
}
@end

NSInteger calculateStrength(NSArray<BridgeComponent *> *bridge) {
    NSInteger strength = 0;
    for (BridgeComponent *component in bridge) {
        strength += component.port1 + component.port2;
    }
    return strength;
}

void findBridges(NSArray<BridgeComponent *> *components, NSInteger currentPort, NSMutableArray<BridgeComponent *> *currentBridge, NSMutableArray<NSArray<BridgeComponent *> *> *bridges) {
    NSInteger maxStrength = 0;
    NSArray<BridgeComponent *> *strongestBridge = nil;
    NSMutableArray<BridgeComponent *> *remainingComponents = [components mutableCopy];

    for (BridgeComponent *component in components) {
        if (component.port1 == currentPort || component.port2 == currentPort) {
            [remainingComponents removeObject:component];
            [currentBridge addObject:component];
            NSInteger nextPort = (component.port1 == currentPort) ? component.port2 : component.port1;
            findBridges(remainingComponents, nextPort, currentBridge, bridges);
            [currentBridge removeLastObject];
            [remainingComponents addObject:component];
        }
    }

    NSInteger currentStrength = calculateStrength(currentBridge);
    if (currentStrength > maxStrength) {
        maxStrength = currentStrength;
        strongestBridge = [currentBridge copy];
    }

    if (strongestBridge) {
        [bridges addObject:strongestBridge];
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];

        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }

        NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSMutableArray<BridgeComponent *> *components = [NSMutableArray array];

        for (NSString *line in lines) {
            if (line.length > 0) {
                NSArray<NSString *> *ports = [line componentsSeparatedByString:@"/"];
                NSInteger port1 = [ports[0] integerValue];
                NSInteger port2 = [ports[1] integerValue];
                [components addObject:[BridgeComponent componentWithPort1:port1 port2:port2]];
            }
        }

        NSMutableArray<NSArray<BridgeComponent *> *> *bridges = [NSMutableArray array];
        findBridges(components, 0, [NSMutableArray array], bridges);

        NSInteger maxStrength = 0;
        NSInteger maxLength = 0;
        NSInteger maxLengthStrength = 0;

        for (NSArray<BridgeComponent *> *bridge in bridges) {
            NSInteger strength = calculateStrength(bridge);
            if (strength > maxStrength) {
                maxStrength = strength;
            }
            if (bridge.count > maxLength) {
                maxLength = bridge.count;
                maxLengthStrength = strength;
            } else if (bridge.count == maxLength) {
                if (strength > maxLengthStrength) {
                    maxLengthStrength = strength;
                }
            }
        }

        NSLog(@"Strongest bridge strength: %ld", (long)maxStrength);
        NSLog(@"Longest bridge strength: %ld", (long)maxLengthStrength);
    }
    return 0;
}