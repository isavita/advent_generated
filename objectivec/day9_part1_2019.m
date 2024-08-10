#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, ParameterMode) {
    PositionMode = 0,
    ImmediateMode = 1,
    RelativeMode = 2
};

typedef NS_ENUM(NSInteger, Opcode) {
    Add = 1,
    Multiply = 2,
    Input = 3,
    Output = 4,
    JumpIfTrue = 5,
    JumpIfFalse = 6,
    LessThan = 7,
    Equals = 8,
    AdjustRelativeBase = 9,
    Halt = 99
};

@interface IntcodeComputer : NSObject
- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program;
- (void)runWithInput:(NSInteger)input;
@end

@implementation IntcodeComputer {
    NSMutableArray<NSNumber *> *memory;
    NSInteger relativeBase;
    NSInteger instructionPointer;
    NSInteger inputValue;
    NSInteger outputValue;
}

- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program {
    self = [super init];
    if (self) {
        memory = [NSMutableArray arrayWithArray:program];
        [memory addObjectsFromArray:[NSMutableArray arrayWithCapacity:10000]]; // Extend memory
        // Ensure the memory array has at least 10000 elements
        while (memory.count < 10000) {
            [memory addObject:@0];
        }
        relativeBase = 0;
        instructionPointer = 0;
        inputValue = 0;
        outputValue = 0;
    }
    return self;
}

- (void)runWithInput:(NSInteger)input {
    inputValue = input;
    while (instructionPointer < memory.count) {
        NSInteger instruction = [memory[instructionPointer] integerValue];
        Opcode opcode = (Opcode)(instruction % 100);
        NSInteger mode1 = (instruction / 100) % 10;
        NSInteger mode2 = (instruction / 1000) % 10;
        NSInteger mode3 = (instruction / 10000) % 10;

        switch (opcode) {
            case Add: {
                NSInteger param1 = [self getParameterValue:instructionPointer + 1 mode:mode1];
                NSInteger param2 = [self getParameterValue:instructionPointer + 2 mode:mode2];
                NSInteger dest = [self getAddress:instructionPointer + 3 mode:mode3];
                memory[dest] = @(param1 + param2);
                instructionPointer += 4;
                break;
            }
            case Multiply: {
                NSInteger param1 = [self getParameterValue:instructionPointer + 1 mode:mode1];
                NSInteger param2 = [self getParameterValue:instructionPointer + 2 mode:mode2];
                NSInteger dest = [self getAddress:instructionPointer + 3 mode:mode3];
                memory[dest] = @(param1 * param2);
                instructionPointer += 4;
                break;
            }
            case Input: {
                NSInteger dest = [self getAddress:instructionPointer + 1 mode:mode1];
                memory[dest] = @(inputValue);
                instructionPointer += 2;
                break;
            }
            case Output: {
                NSInteger param1 = [self getParameterValue:instructionPointer + 1 mode:mode1];
                outputValue = param1;
                NSLog(@"%ld", (long)outputValue);
                instructionPointer += 2;
                break;
            }
            case JumpIfTrue: {
                NSInteger param1 = [self getParameterValue:instructionPointer + 1 mode:mode1];
                NSInteger param2 = [self getParameterValue:instructionPointer + 2 mode:mode2];
                if (param1 != 0) {
                    instructionPointer = param2;
                } else {
                    instructionPointer += 3;
                }
                break;
            }
            case JumpIfFalse: {
                NSInteger param1 = [self getParameterValue:instructionPointer + 1 mode:mode1];
                NSInteger param2 = [self getParameterValue:instructionPointer + 2 mode:mode2];
                if (param1 == 0) {
                    instructionPointer = param2;
                } else {
                    instructionPointer += 3;
                }
                break;
            }
            case LessThan: {
                NSInteger param1 = [self getParameterValue:instructionPointer + 1 mode:mode1];
                NSInteger param2 = [self getParameterValue:instructionPointer + 2 mode:mode2];
                NSInteger dest = [self getAddress:instructionPointer + 3 mode:mode3];
                memory[dest] = @(param1 < param2 ? 1 : 0);
                instructionPointer += 4;
                break;
            }
            case Equals: {
                NSInteger param1 = [self getParameterValue:instructionPointer + 1 mode:mode1];
                NSInteger param2 = [self getParameterValue:instructionPointer + 2 mode:mode2];
                NSInteger dest = [self getAddress:instructionPointer + 3 mode:mode3];
                memory[dest] = @(param1 == param2 ? 1 : 0);
                instructionPointer += 4;
                break;
            }
            case AdjustRelativeBase: {
                NSInteger param1 = [self getParameterValue:instructionPointer + 1 mode:mode1];
                relativeBase += param1;
                instructionPointer += 2;
                break;
            }
            case Halt: {
                return;
            }
            default: {
                NSLog(@"Unknown opcode: %ld", (long)opcode);
                return;
            }
        }
    }
}

- (NSInteger)getParameterValue:(NSInteger)index mode:(NSInteger)mode {
    NSInteger value = [memory[index] integerValue];
    switch (mode) {
        case PositionMode:
            return [memory[value] integerValue];
        case ImmediateMode:
            return value;
        case RelativeMode:
            return [memory[relativeBase + value] integerValue];
        default:
            return 0;
    }
}

- (NSInteger)getAddress:(NSInteger)index mode:(NSInteger)mode {
    NSInteger value = [memory[index] integerValue];
    switch (mode) {
        case PositionMode:
            return value;
        case RelativeMode:
            return relativeBase + value;
        default:
            return 0;
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@","];
        NSMutableArray<NSNumber *> *program = [NSMutableArray array];

        for (NSString *line in lines) {
            [program addObject:@([line integerValue])];
        }

        IntcodeComputer *computer = [[IntcodeComputer alloc] initWithProgram:program];
        [computer runWithInput:1]; // Run in test mode
    }
    return 0;
}