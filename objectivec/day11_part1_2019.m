
#import <Foundation/Foundation.h>

@interface IntcodeComputer : NSObject
- (instancetype)initWithProgram:(long long *)program length:(NSUInteger)len;
- (void)addInput:(long long)value;
- (NSNumber *)runUntilOutput;
- (BOOL)isHalted;
@end

@implementation IntcodeComputer {
    long long *mem;
    NSUInteger memSize;
    NSUInteger ip;
    long long relBase;
    NSMutableArray<NSNumber *> *inQ;
}
- (instancetype)initWithProgram:(long long *)program length:(NSUInteger)len {
    if (self = [super init]) {
        memSize = len * 10;
        mem = calloc(memSize, sizeof(long long));
        memcpy(mem, program, len * sizeof(long long));
        ip = 0; relBase = 0;
        inQ = [NSMutableArray array];
    }
    return self;
}
- (void)dealloc { free(mem); }
- (void)addInput:(long long)value { [inQ addObject:@(value)]; }
- (BOOL)isHalted { return mem[ip] == 99; }
- (long long)getParam:(NSUInteger)offset mode:(int)mode {
    long long p = mem[ip + offset];
    if (mode == 0) return mem[p];
    if (mode == 1) return p;
    return mem[relBase + p];
}
- (void)setParam:(NSUInteger)offset mode:(int)mode value:(long long)v {
    long long p = mem[ip + offset];
    if (mode == 0) mem[p] = v;
    else mem[relBase + p] = v;
}
- (NSNumber *)runUntilOutput {
    while (1) {
        int opcode = mem[ip] % 100;
        int modes = mem[ip] / 100;
        int m1 = modes % 10, m2 = (modes / 10) % 10, m3 = (modes / 100) % 10;
        if (opcode == 99) return nil;
        if (opcode == 1) {
            [self setParam:3 mode:m3 value:[self getParam:1 mode:m1] + [self getParam:2 mode:m2]];
            ip += 4;
        } else if (opcode == 2) {
            [self setParam:3 mode:m3 value:[self getParam:1 mode:m1] * [self getParam:2 mode:m2]];
            ip += 4;
        } else if (opcode == 3) {
            if (inQ.count == 0) return nil;
            [self setParam:1 mode:m1 value:[inQ[0] longLongValue]];
            [inQ removeObjectAtIndex:0];
            ip += 2;
        } else if (opcode == 4) {
            long long out = [self getParam:1 mode:m1];
            ip += 2;
            return @(out);
        } else if (opcode == 5) {
            ip = ([self getParam:1 mode:m1] != 0) ? (NSUInteger)[self getParam:2 mode:m2] : ip + 3;
        } else if (opcode == 6) {
            ip = ([self getParam:1 mode:m1] == 0) ? (NSUInteger)[self getParam:2 mode:m2] : ip + 3;
        } else if (opcode == 7) {
            [self setParam:3 mode:m3 value:([self getParam:1 mode:m1] < [self getParam:2 mode:m2]) ? 1 : 0];
            ip += 4;
        } else if (opcode == 8) {
            [self setParam:3 mode:m3 value:([self getParam:1 mode:m1] == [self getParam:2 mode:m2]) ? 1 : 0];
            ip += 4;
        } else if (opcode == 9) {
            relBase += [self getParam:1 mode:m1];
            ip += 2;
        }
    }
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *tokens = [content componentsSeparatedByString:@","];
        NSUInteger n = tokens.count;
        long long *program = malloc(n * sizeof(long long));
        for (NSUInteger i = 0; i < n; ++i) program[i] = tokens[i].longLongValue;
        IntcodeComputer *cpu = [[IntcodeComputer alloc] initWithProgram:program length:n];
        free(program);
        NSMutableDictionary<NSValue *, NSNumber *> *hull = [NSMutableDictionary dictionary];
        int x = 0, y = 0, dir = 0;
        while (!cpu.isHalted) {
            NSValue *posKey = [NSValue valueWithBytes:&(int[2]){x, y} objCType:@encode(int[2])];
            int cur = hull[posKey] ? hull[posKey].intValue : 0;
            [cpu addInput:cur];
            NSNumber *paint = [cpu runUntilOutput];
            NSNumber *turn  = [cpu runUntilOutput];
            if (!paint || !turn) break;
            hull[posKey] = paint;
            dir = (dir + (turn.intValue == 0 ? -1 : 1) + 4) % 4;
            if (dir == 0) y++;
            else if (dir == 1) x++;
            else if (dir == 2) y--;
            else x--;
        }
        printf("%lu\n", (unsigned long)hull.count);
    }
    return 0;
}
