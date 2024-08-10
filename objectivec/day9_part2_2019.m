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
    NSMutableArray<NSNumber *> *_memory;
    NSInteger _relativeBase;
    NSInteger _instructionPointer;
}

- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program {
    self = [super init];
    if (self) {
        _memory = [NSMutableArray arrayWithArray:program];
        _relativeBase = 0;
        _instructionPointer = 0;
    }
    return self;
}

- (NSInteger)getValueForParameterMode:(ParameterMode)mode atIndex:(NSInteger)index {
    switch (mode) {
        case PositionMode:
            return [self getValueAtIndex:[_memory[index] integerValue]];
        case ImmediateMode:
            return [_memory[index] integerValue];
        case RelativeMode:
            return [self getValueAtIndex:[_memory[index] integerValue] + _relativeBase];
        default:
            return 0;
    }
}

- (void)setValue:(NSInteger)value forParameterMode:(ParameterMode)mode atIndex:(NSInteger)index {
    NSInteger address;
    switch (mode) {
        case PositionMode:
            address = [_memory[index] integerValue];
            break;
        case RelativeMode:
            address = [_memory[index] integerValue] + _relativeBase;
            break;
        default:
            return;
    }
    [self setValue:value atIndex:address];
}

- (NSInteger)getValueAtIndex:(NSInteger)index {
    while (index >= _memory.count) {
        [_memory addObject:@0];
    }
    return [_memory[index] integerValue];
}

- (void)setValue:(NSInteger)value atIndex:(NSInteger)index {
    while (index >= _memory.count) {
        [_memory addObject:@0];
    }
    _memory[index] = @(value);
}

- (void)runWithInput:(NSInteger)input {
    while (true) {
        NSInteger instruction = [self getValueAtIndex:_instructionPointer];
        NSInteger opcode = instruction % 100;
        NSInteger mode1 = (instruction / 100) % 10;
        NSInteger mode2 = (instruction / 1000) % 10;
        NSInteger mode3 = (instruction / 10000) % 10;

        switch (opcode) {
            case Add: {
                NSInteger param1 = [self getValueForParameterMode:mode1 atIndex:_instructionPointer + 1];
                NSInteger param2 = [self getValueForParameterMode:mode2 atIndex:_instructionPointer + 2];
                [self setValue:param1 + param2 forParameterMode:mode3 atIndex:_instructionPointer + 3];
                _instructionPointer += 4;
                break;
            }
            case Multiply: {
                NSInteger param1 = [self getValueForParameterMode:mode1 atIndex:_instructionPointer + 1];
                NSInteger param2 = [self getValueForParameterMode:mode2 atIndex:_instructionPointer + 2];
                [self setValue:param1 * param2 forParameterMode:mode3 atIndex:_instructionPointer + 3];
                _instructionPointer += 4;
                break;
            }
            case Input: {
                [self setValue:input forParameterMode:mode1 atIndex:_instructionPointer + 1];
                _instructionPointer += 2;
                break;
            }
            case Output: {
                NSInteger outputValue = [self getValueForParameterMode:mode1 atIndex:_instructionPointer + 1];
                printf("%ld\n", (long)outputValue);
                _instructionPointer += 2;
                break;
            }
            case JumpIfTrue: {
                NSInteger param1 = [self getValueForParameterMode:mode1 atIndex:_instructionPointer + 1];
                NSInteger param2 = [self getValueForParameterMode:mode2 atIndex:_instructionPointer + 2];
                if (param1 != 0) {
                    _instructionPointer = param2;
                } else {
                    _instructionPointer += 3;
                }
                break;
            }
            case JumpIfFalse: {
                NSInteger param1 = [self getValueForParameterMode:mode1 atIndex:_instructionPointer + 1];
                NSInteger param2 = [self getValueForParameterMode:mode2 atIndex:_instructionPointer + 2];
                if (param1 == 0) {
                    _instructionPointer = param2;
                } else {
                    _instructionPointer += 3;
                }
                break;
            }
            case LessThan: {
                NSInteger param1 = [self getValueForParameterMode:mode1 atIndex:_instructionPointer + 1];
                NSInteger param2 = [self getValueForParameterMode:mode2 atIndex:_instructionPointer + 2];
                [self setValue:(param1 < param2) ? 1 : 0 forParameterMode:mode3 atIndex:_instructionPointer + 3];
                _instructionPointer += 4;
                break;
            }
            case Equals: {
                NSInteger param1 = [self getValueForParameterMode:mode1 atIndex:_instructionPointer + 1];
                NSInteger param2 = [self getValueForParameterMode:mode2 atIndex:_instructionPointer + 2];
                [self setValue:(param1 == param2) ? 1 : 0 forParameterMode:mode3 atIndex:_instructionPointer + 3];
                _instructionPointer += 4;
                break;
            }
            case AdjustRelativeBase: {
                NSInteger param1 = [self getValueForParameterMode:mode1 atIndex:_instructionPointer + 1];
                _relativeBase += param1;
                _instructionPointer += 2;
                break;
            }
            case Halt:
                return;
            default:
                NSLog(@"Unknown opcode: %ld", (long)opcode);
                return;
        }
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@","];
        NSMutableArray<NSNumber *> *program = [NSMutableArray arrayWithCapacity:lines.count];
        for (NSString *line in lines) {
            [program addObject:@([line integerValue])];
        }

        IntcodeComputer *computer = [[IntcodeComputer alloc] initWithProgram:program];
        NSInteger input = 2; // Change to 1 for test mode
        [computer runWithInput:input];
    }
    return 0;
}