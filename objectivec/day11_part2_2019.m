
#import <Foundation/Foundation.h>

@interface IntcodeComputer : NSObject
@property (nonatomic, assign) BOOL halted;
- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program;
- (NSNumber *)runStepWithInput:(long long)input;
@end

@implementation IntcodeComputer {
    NSMutableDictionary<NSNumber *, NSNumber *> *_memory;
    long long _ip;
    long long _relativeBase;
}

- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program {
    if (self = [super init]) {
        _memory = [NSMutableDictionary dictionary];
        [program enumerateObjectsUsingBlock:^(NSNumber *num, NSUInteger idx, BOOL *stop) {
            _memory[@(idx)] = num;
        }];
        _ip = 0;
        _relativeBase = 0;
        _halted = NO;
    }
    return self;
}

- (long long)getParameterWithMode:(int)mode offset:(int)offset {
    long long param = [_memory[@(_ip + offset)] longLongValue];
    switch (mode) {
        case 0: return [_memory[@(param)] longLongValue];
        case 1: return param;
        case 2: return [_memory[@(_relativeBase + param)] longLongValue];
        default: return 0;
    }
}

- (void)setParameterWithMode:(int)mode offset:(int)offset value:(long long)value {
    long long param = [_memory[@(_ip + offset)] longLongValue];
    if (mode == 0) {
        _memory[@(param)] = @(value);
    } else if (mode == 2) {
        _memory[@(_relativeBase + param)] = @(value);
    }
}

- (NSNumber *)runStepWithInput:(long long)input {
    while (YES) {
        long long instruction = [_memory[@(_ip)] longLongValue];
        int opcode = instruction % 100;
        int mode1 = (instruction / 100) % 10;
        int mode2 = (instruction / 1000) % 10;
        int mode3 = (instruction / 10000) % 10;
        switch (opcode) {
            case 1: {
                long long v = [self getParameterWithMode:mode1 offset:1] + [self getParameterWithMode:mode2 offset:2];
                [self setParameterWithMode:mode3 offset:3 value:v];
                _ip += 4;
                break;
            }
            case 2: {
                long long v = [self getParameterWithMode:mode1 offset:1] * [self getParameterWithMode:mode2 offset:2];
                [self setParameterWithMode:mode3 offset:3 value:v];
                _ip += 4;
                break;
            }
            case 3: {
                [self setParameterWithMode:mode1 offset:1 value:input];
                _ip += 2;
                break;
            }
            case 4: {
                long long out = [self getParameterWithMode:mode1 offset:1];
                _ip += 2;
                return @(out);
            }
            case 5: {
                _ip = [self getParameterWithMode:mode1 offset:1] != 0 ? [self getParameterWithMode:mode2 offset:2] : _ip + 3;
                break;
            }
            case 6: {
                _ip = [self getParameterWithMode:mode1 offset:1] == 0 ? [self getParameterWithMode:mode2 offset:2] : _ip + 3;
                break;
            }
            case 7: {
                long long v = [self getParameterWithMode:mode1 offset:1] < [self getParameterWithMode:mode2 offset:2] ? 1 : 0;
                [self setParameterWithMode:mode3 offset:3 value:v];
                _ip += 4;
                break;
            }
            case 8: {
                long long v = [self getParameterWithMode:mode1 offset:1] == [self getParameterWithMode:mode2 offset:2] ? 1 : 0;
                [self setParameterWithMode:mode3 offset:3 value:v];
                _ip += 4;
                break;
            }
            case 9: {
                _relativeBase += [self getParameterWithMode:mode1 offset:1];
                _ip += 2;
                break;
            }
            case 99: {
                _halted = YES;
                return nil;
            }
            default: {
                _halted = YES;
                return nil;
            }
        }
    }
}

@end

@interface Robot : NSObject
- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program startPanelColor:(int)color;
- (void)run;
- (NSUInteger)paintedPanelsCount;
- (void)renderPanels;
@end

@implementation Robot {
    IntcodeComputer *_computer;
    int _direction; // 0 up,1 right,2 down,3 left
    int _x, _y;
    NSMutableDictionary<NSString *, NSNumber *> *_panels;
    NSMutableSet<NSString *> *_painted;
}

- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program startPanelColor:(int)color {
    if (self = [super init]) {
        _computer = [[IntcodeComputer alloc] initWithProgram:program];
        _direction = 0;
        _x = 0; _y = 0;
        _panels = [NSMutableDictionary dictionary];
        _painted = [NSMutableSet set];
        _panels[[NSString stringWithFormat:@"%d,%d", _x, _y]] = @(color);
    }
    return self;
}

- (long long)panelColorAtX:(int)x y:(int)y {
    NSNumber *v = _panels[[NSString stringWithFormat:@"%d,%d", x, y]];
    return v ? v.longLongValue : 0;
}

- (void)setPanelAtX:(int)x y:(int)y color:(int)color {
    NSString *key = [NSString stringWithFormat:@"%d,%d", x, y];
    _panels[key] = @(color);
    [_painted addObject:key];
}

- (void)turnAndMove:(int)turn {
    _direction = (_direction + (turn == 0 ? -1 : 1) + 4) % 4;
    switch (_direction) {
        case 0: _y -= 1; break;
        case 1: _x += 1; break;
        case 2: _y += 1; break;
        case 3: _x -= 1; break;
    }
}

- (void)run {
    while (!_computer.halted) {
        long long input = [self panelColorAtX:_x y:_y];
        NSNumber *paintNum = [_computer runStepWithInput:input];
        if (!_computer.halted && paintNum) {
            int paint = paintNum.intValue;
            NSNumber *turnNum = [_computer runStepWithInput:0];
            if (!_computer.halted && turnNum) {
                int turn = turnNum.intValue;
                [self setPanelAtX:_x y:_y color:paint];
                [self turnAndMove:turn];
            }
        }
    }
}

- (NSUInteger)paintedPanelsCount {
    return _painted.count;
}

- (void)renderPanels {
    if (_panels.count == 0) return;
    int minX = INT_MAX, maxX = INT_MIN, minY = INT_MAX, maxY = INT_MIN;
    for (NSString *key in _panels) {
        NSArray<NSString *> *parts = [key componentsSeparatedByString:@","];
        int x = parts[0].intValue;
        int y = parts[1].intValue;
        if (x < minX) minX = x;
        if (x > maxX) maxX = x;
        if (y < minY) minY = y;
        if (y > maxY) maxY = y;
    }
    printf("\nRegistration Identifier:\n");
    for (int y = minY; y <= maxY; ++y) {
        for (int x = minX; x <= maxX; ++x) {
            long long v = [self panelColorAtX:x y:y];
            putchar(v == 1 ? '#' : ' ');
        }
        putchar('\n');
    }
}

@end

static NSArray<NSNumber *> *parseInput(NSString *path) {
    NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString *> *parts = [content componentsSeparatedByString:@","];
    NSMutableArray<NSNumber *> *nums = [NSMutableArray array];
    for (NSString *p in parts) {
        if (p.length) [nums addObject:@(p.longLongValue)];
    }
    return nums;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<NSNumber *> *program = parseInput(@"input.txt");
        Robot *robot1 = [[Robot alloc] initWithProgram:program startPanelColor:0];
        [robot1 run];
        printf("Part One: %lu panels painted at least once.\n", (unsigned long)[robot1 paintedPanelsCount]);
        Robot *robot2 = [[Robot alloc] initWithProgram:program startPanelColor:1];
        [robot2 run];
        printf("Part Two: Registration identifier painted on the hull.\n");
        [robot2 renderPanels];
    }
    return 0;
}
