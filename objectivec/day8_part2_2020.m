#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, OperationType) {
    acc,
    jmp,
    nop
};

@interface Instruction : NSObject
@property (nonatomic) OperationType operation;
@property (nonatomic) NSInteger argument;
@end

@implementation Instruction
- (instancetype)initWithOperation:(OperationType)operation argument:(NSInteger)argument {
    self = [super init];
    if (self) {
        _operation = operation;
        _argument = argument;
    }
    return self;
}
@end

NSInteger executeProgram(NSArray<Instruction *> *instructions, BOOL *terminated) {
    NSMutableSet<NSNumber *> *visitedIndices = [NSMutableSet set];
    NSInteger accumulator = 0;
    NSInteger currentIndex = 0;

    while (currentIndex < instructions.count) {
        if ([visitedIndices containsObject:@(currentIndex)]) {
            *terminated = NO;
            return accumulator;
        }
        [visitedIndices addObject:@(currentIndex)];

        Instruction *instruction = instructions[currentIndex];
        switch (instruction.operation) {
            case acc:
                accumulator += instruction.argument;
                currentIndex++;
                break;
            case jmp:
                currentIndex += instruction.argument;
                break;
            case nop:
                currentIndex++;
                break;
        }
    }

    *terminated = YES;
    return accumulator;
}

NSInteger fixAndExecuteProgram(NSArray<Instruction *> *instructions) {
    for (NSInteger i = 0; i < instructions.count; i++) {
        Instruction *originalInstruction = instructions[i];
        Instruction *modifiedInstruction;

        if (originalInstruction.operation == jmp) {
            modifiedInstruction = [[Instruction alloc] initWithOperation:nop argument:originalInstruction.argument];
        } else if (originalInstruction.operation == nop) {
            modifiedInstruction = [[Instruction alloc] initWithOperation:jmp argument:originalInstruction.argument];
        } else {
            continue;
        }

        NSMutableArray<Instruction *> *modifiedInstructions = [instructions mutableCopy];
        modifiedInstructions[i] = modifiedInstruction;

        BOOL terminated;
        NSInteger accumulator = executeProgram(modifiedInstructions, &terminated);
        if (terminated) {
            return accumulator;
        }
    }

    return -1; // Should never reach here
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@"\n"];

        NSMutableArray<Instruction *> *instructions = [NSMutableArray array];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray<NSString *> *components = [line componentsSeparatedByString:@" "];
            OperationType operation;
            if ([components[0] isEqualToString:@"acc"]) {
                operation = acc;
            } else if ([components[0] isEqualToString:@"jmp"]) {
                operation = jmp;
            } else if ([components[0] isEqualToString:@"nop"]) {
                operation = nop;
            } else {
                continue;
            }
            NSInteger argument = [components[1] integerValue];
            Instruction *instruction = [[Instruction alloc] initWithOperation:operation argument:argument];
            [instructions addObject:instruction];
        }

        BOOL terminated;
        NSInteger accumulatorBeforeLoop = executeProgram(instructions, &terminated);
        NSLog(@"Accumulator value before second execution: %ld", (long)accumulatorBeforeLoop);

        NSInteger accumulatorAfterFix = fixAndExecuteProgram(instructions);
        NSLog(@"Accumulator value after fixing the program: %ld", (long)accumulatorAfterFix);
    }
    return 0;
}