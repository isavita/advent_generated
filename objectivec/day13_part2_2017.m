
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *firewall = [NSMutableDictionary dictionary];
        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@": "];
            firewall[components[0]] = components[1];
        }
        
        int severity = 0;
        for (NSString *depth in firewall.allKeys) {
            int range = [firewall[depth] intValue];
            if ([depth intValue] % ((range - 1) * 2) == 0) {
                severity += [depth intValue] * range;
            }
        }
        
        printf("Part 1: %d\n", severity);
        
        int delay = 0;
        while (true) {
            BOOL caught = NO;
            for (NSString *depth in firewall.allKeys) {
                int range = [firewall[depth] intValue];
                if (([depth intValue] + delay) % ((range - 1) * 2) == 0) {
                    caught = YES;
                    break;
                }
            }
            if (!caught) {
                break;
            }
            delay++;
        }
        
        printf("Part 2: %d\n", delay);
    }
    return 0;
}
