#import <Foundation/Foundation.h>

int executeBootCode(NSArray<NSString *> *instructions) {
    int accumulator = 0, currentInstruction = 0;
    NSMutableSet<NSNumber *> *visited = [NSMutableSet set];

    while (currentInstruction < instructions.count) {
        if ([visited containsObject:@(currentInstruction)]) return accumulator;

        [visited addObject:@(currentInstruction)];
        NSArray<NSString *> *parts = [instructions[currentInstruction] componentsSeparatedByString:@" "];
        NSString *op = parts[0];
        int arg = [parts[1] intValue];

        if ([op isEqualToString:@"acc"]) {
            accumulator += arg;
            currentInstruction++;
        } else if ([op isEqualToString:@"jmp"]) {
            currentInstruction += arg;
        } else {
            currentInstruction++;
        }
    }
    return accumulator;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }

        NSArray<NSString *> *instructions = [fileContents componentsSeparatedByString:@"\n"];
        int accumulator = executeBootCode(instructions);
        NSLog(@"%d", accumulator);
    }
    return 0;
}