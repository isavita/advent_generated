#import <Foundation/Foundation.h>

typedef enum {
    TileEmpty = 0,
    TileWall = 1,
    TileBlock = 2,
    TilePaddle = 3,
    TileBall = 4
} TileType;

@interface IntcodeComputer : NSObject
- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program;
- (void)runProgram;
- (NSArray<NSNumber *> *)getOutput;
@end

@implementation IntcodeComputer {
    NSMutableArray<NSNumber *> *_program;
    NSInteger _pointer;
    NSInteger _relativeBase;
    NSMutableArray<NSNumber *> *_output;
}

- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program {
    self = [super init];
    if (self) {
        _program = [program mutableCopy];
        _pointer = 0;
        _relativeBase = 0;
        _output = [NSMutableArray array];
    }
    return self;
}

- (NSInteger)getValueAtIndex:(NSInteger)index mode:(NSInteger)mode {
    switch (mode) {
        case 0:
            return [_program[[_program[index] integerValue]] integerValue];
        case 1:
            return [_program[index] integerValue];
        case 2:
            return [_program[[_program[index] integerValue] + _relativeBase] integerValue];
        default:
            return 0;
    }
}

- (void)setValue:(NSInteger)value atIndex:(NSInteger)index mode:(NSInteger)mode {
    NSInteger address;
    switch (mode) {
        case 0:
            address = [_program[index] integerValue];
            break;
        case 2:
            address = [_program[index] integerValue] + _relativeBase;
            break;
        default:
            return;
    }
    while (address >= _program.count) {
        [_program addObject:@0];
    }
    _program[address] = @(value);
}

- (void)runProgram {
    while (_pointer < _program.count) {
        NSInteger instruction = [_program[_pointer] integerValue];
        NSInteger opcode = instruction % 100;
        NSInteger mode1 = (instruction / 100) % 10;
        NSInteger mode2 = (instruction / 1000) % 10;
        NSInteger mode3 = (instruction / 10000) % 10;

        switch (opcode) {
            case 1: {
                NSInteger param1 = [self getValueAtIndex:_pointer + 1 mode:mode1];
                NSInteger param2 = [self getValueAtIndex:_pointer + 2 mode:mode2];
                [self setValue:param1 + param2 atIndex:_pointer + 3 mode:mode3];
                _pointer += 4;
                break;
            }
            case 2: {
                NSInteger param1 = [self getValueAtIndex:_pointer + 1 mode:mode1];
                NSInteger param2 = [self getValueAtIndex:_pointer + 2 mode:mode2];
                [self setValue:param1 * param2 atIndex:_pointer + 3 mode:mode3];
                _pointer += 4;
                break;
            }
            case 3: {
                // For this challenge, we don't need to handle input
                _pointer += 2;
                break;
            }
            case 4: {
                NSInteger outputValue = [self getValueAtIndex:_pointer + 1 mode:mode1];
                [_output addObject:@(outputValue)];
                _pointer += 2;
                break;
            }
            case 5: {
                NSInteger param1 = [self getValueAtIndex:_pointer + 1 mode:mode1];
                NSInteger param2 = [self getValueAtIndex:_pointer + 2 mode:mode2];
                if (param1 != 0) {
                    _pointer = param2;
                } else {
                    _pointer += 3;
                }
                break;
            }
            case 6: {
                NSInteger param1 = [self getValueAtIndex:_pointer + 1 mode:mode1];
                NSInteger param2 = [self getValueAtIndex:_pointer + 2 mode:mode2];
                if (param1 == 0) {
                    _pointer = param2;
                } else {
                    _pointer += 3;
                }
                break;
            }
            case 7: {
                NSInteger param1 = [self getValueAtIndex:_pointer + 1 mode:mode1];
                NSInteger param2 = [self getValueAtIndex:_pointer + 2 mode:mode2];
                [self setValue:(param1 < param2) ? 1 : 0 atIndex:_pointer + 3 mode:mode3];
                _pointer += 4;
                break;
            }
            case 8: {
                NSInteger param1 = [self getValueAtIndex:_pointer + 1 mode:mode1];
                NSInteger param2 = [self getValueAtIndex:_pointer + 2 mode:mode2];
                [self setValue:(param1 == param2) ? 1 : 0 atIndex:_pointer + 3 mode:mode3];
                _pointer += 4;
                break;
            }
            case 9: {
                NSInteger param1 = [self getValueAtIndex:_pointer + 1 mode:mode1];
                _relativeBase += param1;
                _pointer += 2;
                break;
            }
            case 99:
                return;
            default:
                NSLog(@"Unknown opcode: %ld", (long)opcode);
                return;
        }
    }
}

- (NSArray<NSNumber *> *)getOutput {
    return _output;
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
        [computer runProgram];

        NSArray<NSNumber *> *output = [computer getOutput];
        NSUInteger blockTileCount = 0;

        for (NSUInteger i = 0; i + 2 < output.count; i += 3) {
            NSInteger x = [output[i] integerValue];
            NSInteger y = [output[i + 1] integerValue];
            NSInteger tileId = [output[i + 2] integerValue];

            if (tileId == TileBlock) {
                blockTileCount++;
            }
        }

        NSLog(@"Number of block tiles: %lu", (unsigned long)blockTileCount);
    }
    return 0;
}