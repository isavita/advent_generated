#import <Foundation/Foundation.h>

int main(int argc, char *argv[]) {
    // Read input from file
    NSString *inputPath = @"input.txt";
    NSString *input = [NSString stringWithContentsOfFile:inputPath encoding:NSUTF8StringEncoding error:nil];

    // Parse input into layers
    NSArray *layers = [input componentsSeparatedByString:@"\n"];
    NSMutableDictionary *firewall = [NSMutableDictionary dictionary];
    for (NSString *layer in layers) {
        if ([layer length] == 0) continue;
        NSArray *components = [layer componentsSeparatedByString:@":"];
        NSUInteger depth = [components[0] integerValue];
        NSUInteger range = [components[1] integerValue];
        firewall[@(depth)] = @(range);
    }

    // Simulate trip through firewall
    NSUInteger severity = 0;
    for (NSUInteger depth = 0; depth <= [firewall.allKeys.lastObject integerValue]; depth++) {
        NSUInteger range = [firewall[@(depth)] integerValue];
        if (range == 0) continue;
        NSUInteger scannerPosition = depth % (2 * range - 2);
        if (scannerPosition == 0) {
            severity += depth * range;
        }
    }

    // Print result
    NSLog(@"Severity: %lu", severity);

    return 0;
}