#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *pipes = [NSMutableDictionary dictionary];
        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" <-> "];
            NSInteger program = [parts[0] integerValue];
            NSArray *connectedPrograms = [parts[1] componentsSeparatedByString:@", "];
            pipes[@(program)] = connectedPrograms;
        }
        
        NSMutableSet *group = [NSMutableSet set];
        [group addObject:@(0)];
        BOOL updated = YES;
        while (updated) {
            updated = NO;
            for (NSNumber *program in [group copy]) {
                NSArray *connectedPrograms = pipes[program];
                for (NSString *connectedProgram in connectedPrograms) {
                    if (![group containsObject:@(connectedProgram.integerValue)]) {
                        [group addObject:@(connectedProgram.integerValue)];
                        updated = YES;
                    }
                }
            }
        }
        
        printf("%lu\n", (unsigned long)group.count);
    }
    return 0;
}