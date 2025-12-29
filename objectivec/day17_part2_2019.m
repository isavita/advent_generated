
#import <Foundation/Foundation.h>

@interface IntcodeVM : NSObject
@property (nonatomic, strong) NSMutableDictionary<NSNumber *, NSNumber *> *memory;
@property (nonatomic) long long pc;
@property (nonatomic) long long relativeBase;
@property (nonatomic, strong) NSMutableArray<NSNumber *> *inputs;
@property (nonatomic, strong) NSMutableArray<NSNumber *> *outputs;
@property (nonatomic) BOOL halted;

- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program;
- (void)run;
@end

@implementation IntcodeVM

- (instancetype)initWithProgram:(NSArray<NSNumber *> *)program {
    self = [super init];
    if (self) {
        _memory = [NSMutableDictionary dictionary];
        for (NSInteger i = 0; i < program.count; i++) {
            _memory[@(i)] = program[i];
        }
        _pc = 0;
        _relativeBase = 0;
        _inputs = [NSMutableArray array];
        _outputs = [NSMutableArray array];
        _halted = NO;
    }
    return self;
}

- (long long)readMem:(long long)addr {
    return [self.memory[@(addr)] longLongValue];
}

- (void)writeMem:(long long)addr value:(long long)val {
    self.memory[@(addr)] = @(val);
}

- (long long)getParamAddr:(int)mode offset:(int)offset {
    long long val = [self readMem:self.pc + offset];
    if (mode == 0) return val;
    if (mode == 1) return self.pc + offset;
    if (mode == 2) return self.relativeBase + val;
    return 0;
}

- (void)run {
    while (YES) {
        long long instruction = [self readMem:self.pc];
        int opcode = instruction % 100;
        int m1 = (instruction / 100) % 10;
        int m2 = (instruction / 1000) % 10;
        int m3 = (instruction / 10000) % 10;

        if (opcode == 99) { self.halted = YES; break; }

        if (opcode == 1) {
            [self writeMem:[self getParamAddr:m3 offset:3] value:[self readMem:[self getParamAddr:m1 offset:1]] + [self readMem:[self getParamAddr:m2 offset:2]]];
            self.pc += 4;
        } else if (opcode == 2) {
            [self writeMem:[self getParamAddr:m3 offset:3] value:[self readMem:[self getParamAddr:m1 offset:1]] * [self readMem:[self getParamAddr:m2 offset:2]]];
            self.pc += 4;
        } else if (opcode == 3) {
            if (self.inputs.count == 0) return;
            [self writeMem:[self getParamAddr:m1 offset:1] value:[self.inputs.firstObject longLongValue]];
            [self.inputs removeObjectAtIndex:0];
            self.pc += 2;
        } else if (opcode == 4) {
            [self.outputs addObject:@([self readMem:[self getParamAddr:m1 offset:1]])];
            self.pc += 2;
        } else if (opcode == 5) {
            if ([self readMem:[self getParamAddr:m1 offset:1]] != 0) self.pc = [self readMem:[self getParamAddr:m2 offset:2]];
            else self.pc += 3;
        } else if (opcode == 6) {
            if ([self readMem:[self getParamAddr:m1 offset:1]] == 0) self.pc = [self readMem:[self getParamAddr:m2 offset:2]];
            else self.pc += 3;
        } else if (opcode == 7) {
            [self writeMem:[self getParamAddr:m3 offset:3] value:([self readMem:[self getParamAddr:m1 offset:1]] < [self readMem:[self getParamAddr:m2 offset:2]] ? 1 : 0)];
            self.pc += 4;
        } else if (opcode == 8) {
            [self writeMem:[self getParamAddr:m3 offset:3] value:([self readMem:[self getParamAddr:m1 offset:1]] == [self readMem:[self getParamAddr:m2 offset:2]] ? 1 : 0)];
            self.pc += 4;
        } else if (opcode == 9) {
            self.relativeBase += [self readMem:[self getParamAddr:m1 offset:1]];
            self.pc += 2;
        }
    }
}
@end

// Path compression logic
BOOL findDecomposition(NSArray *path, NSMutableArray *functions, NSMutableArray *mainRoutine) {
    if (path.count == 0) return YES;
    
    // Try existing functions
    for (int i = 0; i < functions.count; i++) {
        NSArray *f = functions[i];
        if (path.count >= f.count && [[path subarrayWithRange:NSMakeRange(0, f.count)] isEqualToArray:f]) {
            [mainRoutine addObject:@[@"A", @"B", @"C"][i]];
            if (findDecomposition([path subarrayWithRange:NSMakeRange(f.count, path.count - f.count)], functions, mainRoutine)) return YES;
            [mainRoutine removeLastObject];
        }
    }
    
    // Try creating a new function
    if (functions.count < 3) {
        for (int len = 1; len <= path.count; len++) {
            NSArray *newF = [path subarrayWithRange:NSMakeRange(0, len)];
            if ([newF componentsJoinedByString:@","].length > 20) break;
            [functions addObject:newF];
            [mainRoutine addObject:@[@"A", @"B", @"C"][functions.count - 1]];
            if (findDecomposition([path subarrayWithRange:NSMakeRange(len, path.count - len)], functions, mainRoutine)) return YES;
            [mainRoutine removeLastObject];
            [functions removeLastObject];
        }
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (!content) return 1;
        
        NSMutableArray<NSNumber *> *program = [NSMutableArray array];
        for (NSString *s in [content componentsSeparatedByString:@","]) {
            [program addObject:@([s longLongValue])];
        }
        
        // Part 1
        IntcodeVM *vm = [[IntcodeVM alloc] initWithProgram:program];
        [vm run];
        
        NSMutableArray<NSString *> *grid = [NSMutableArray array];
        NSMutableString *row = [NSMutableString string];
        for (NSNumber *n in vm.outputs) {
            char c = (char)[n intValue];
            if (c == '\n') {
                if (row.length > 0) [grid addObject:[row copy]];
                [row setString:@""];
            } else [row appendFormat:@"%c", c];
        }
        
        long long alignmentSum = 0;
        int height = (int)grid.count;
        int width = (int)grid[0].length;
        int robotX = 0, robotY = 0, dir = 0;
        
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                char c = [grid[y] characterAtIndex:x];
                if (y > 0 && y < height - 1 && x > 0 && x < width - 1) {
                    if (c == '#' && [grid[y-1] characterAtIndex:x] == '#' && [grid[y+1] characterAtIndex:x] == '#' &&
                        [grid[y] characterAtIndex:x-1] == '#' && [grid[y] characterAtIndex:x+1] == '#') {
                        alignmentSum += x * y;
                    }
                }
                if (c == '^' || c == 'v' || c == '<' || c == '>') {
                    robotX = x; robotY = y;
                    dir = (c == '^') ? 0 : (c == '>') ? 1 : (c == 'v') ? 2 : 3;
                }
            }
        }
        printf("Part 1: %lld\n", alignmentSum);
        
        // Part 2: Trace path
        int dx[] = {0, 1, 0, -1}, dy[] = {-1, 0, 1, 0};
        NSMutableArray *path = [NSMutableArray array];
        while (YES) {
            int steps = 0;
            while (robotY+dy[dir] >= 0 && robotY+dy[dir] < height && robotX+dx[dir] >= 0 && robotX+dx[dir] < width && [grid[robotY+dy[dir]] characterAtIndex:robotX+dx[dir]] == '#') {
                robotX += dx[dir]; robotY += dy[dir]; steps++;
            }
            if (steps > 0) [path addObject:[NSString stringWithFormat:@"%d", steps]];
            
            if (robotY+dy[(dir+3)%4] >= 0 && robotY+dy[(dir+3)%4] < height && robotX+dx[(dir+3)%4] >= 0 && robotX+dx[(dir+3)%4] < width && [grid[robotY+dy[(dir+3)%4]] characterAtIndex:robotX+dx[(dir+3)%4]] == '#') {
                dir = (dir+3)%4; [path addObject:@"L"];
            } else if (robotY+dy[(dir+1)%4] >= 0 && robotY+dy[(dir+1)%4] < height && robotX+dx[(dir+1)%4] >= 0 && robotX+dx[(dir+1)%4] < width && [grid[robotY+dy[(dir+1)%4]] characterAtIndex:robotX+dx[(dir+1)%4]] == '#') {
                dir = (dir+1)%4; [path addObject:@"R"];
            } else break;
        }
        
        NSMutableArray *functions = [NSMutableArray array];
        NSMutableArray *mainRoutine = [NSMutableArray array];
        findDecomposition(path, functions, mainRoutine);
        
        NSString *inputStr = [NSString stringWithFormat:@"%@\n%@\n%@\n%@\nn\n", 
                             [mainRoutine componentsJoinedByString:@","],
                             [functions[0] componentsJoinedByString:@","],
                             [functions[1] componentsJoinedByString:@","],
                             [functions[2] componentsJoinedByString:@","]];
        
        vm = [[IntcodeVM alloc] initWithProgram:program];
        [vm writeMem:0 value:2];
        for (int i = 0; i < inputStr.length; i++) {
            [vm.inputs addObject:@([inputStr characterAtIndex:i])];
        }
        [vm run];
        printf("Part 2: %lld\n", [vm.outputs.lastObject longLongValue]);
    }
    return 0;
}
