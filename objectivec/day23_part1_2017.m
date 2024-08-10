#import <Foundation/Foundation.h>

@interface Instruction : NSObject
@property (nonatomic, strong) NSString *op;
@property (nonatomic, strong) NSString *arg1;
@property (nonatomic, strong) NSString *arg2;
@end

@implementation Instruction
@end

@interface Processor : NSObject
@property (nonatomic) NSUInteger mulCount;
@property (nonatomic) NSMutableDictionary *registers;

- (void)executeInstructions:(NSArray<Instruction *> *)instructions;
@end

@implementation Processor

- (instancetype)init {
    self = [super init];
    if (self) {
        _registers = [NSMutableDictionary dictionaryWithCapacity:8];
        for (char c = 'a'; c <= 'h'; c++) {
            _registers[[NSString stringWithFormat:@"%c", c]] = @0;
        }
    }
    return self;
}

- (void)executeInstructions:(NSArray<Instruction *> *)instructions {
    self.mulCount = 0;
    NSUInteger pc = 0;
    while (pc < instructions.count) {
        Instruction *instruction = instructions[pc];
        NSString *op = instruction.op;
        NSString *arg1 = instruction.arg1;
        NSString *arg2 = instruction.arg2;

        NSInteger value1 = [self valueForArgument:arg1];
        NSInteger value2 = [self valueForArgument:arg2];

        if ([op isEqualToString:@"set"]) {
            self.registers[arg1] = @(value2);
        } else if ([op isEqualToString:@"sub"]) {
            self.registers[arg1] = @(value1 - value2);
        } else if ([op isEqualToString:@"mul"]) {
            self.registers[arg1] = @(value1 * value2);
            self.mulCount++;
        } else if ([op isEqualToString:@"jnz"]) {
            if (value1 != 0) {
                pc += value2 - 1; // -1 to counteract the pc++ at the end of the loop
            }
        }
        pc++;
    }
}

- (NSInteger)valueForArgument:(NSString *)arg {
    if ([arg rangeOfCharacterFromSet:[NSCharacterSet decimalDigitCharacterSet]].location != NSNotFound) {
        return [arg integerValue];
    } else {
        return [self.registers[arg] integerValue];
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@"\n"];

        NSMutableArray<Instruction *> *instructions = [NSMutableArray arrayWithCapacity:lines.count];

        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray<NSString *> *components = [line componentsSeparatedByString:@" "];
            if (components.count == 3) {
                Instruction *instruction = [[Instruction alloc] init];
                instruction.op = components[0];
                instruction.arg1 = components[1];
                instruction.arg2 = components[2];
                [instructions addObject:instruction];
            }
        }

        Processor *processor = [[Processor alloc] init];
        [processor executeInstructions:instructions];

        NSLog(@"The mul instruction was invoked %lu times.", (unsigned long)processor.mulCount);
    }
    return 0;
}