#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, Mode) {
    Position,
    Immediate,
    Relative
};

@interface Cmd : NSObject
@property (nonatomic) NSInteger value;
- (NSInteger)opCode;
- (NSArray<NSNumber *> *)modesWithArity:(NSInteger)arity;
@end

@implementation Cmd
- (NSInteger)opCode {
    return self.value % 100;
}

- (NSArray<NSNumber *> *)modesWithArity:(NSInteger)arity {
    NSInteger modeSection = self.value / 100;
    NSMutableArray<NSNumber *> *modes = [NSMutableArray arrayWithCapacity:arity];
    for (NSInteger i = 0; i < arity; i++) {
        [modes addObject:@(modeSection / (NSInteger)pow(10, i) % 10)];
    }
    return modes;
}
@end

@interface VM : NSObject
@property (nonatomic) NSMutableDictionary<NSNumber *, NSNumber *> *code;
@property (nonatomic) NSInteger ip;
@property (nonatomic) NSInteger relativeBase;
@property (nonatomic) NSMutableArray<NSNumber *> *input;
@property (nonatomic) NSMutableArray<NSNumber *> *output;

- (instancetype)initWithFile:(NSString *)filename;
- (void)run;
- (NSArray<NSNumber *> *)getParamsAddresses:(NSInteger)pos cmd:(Cmd *)cmd arity:(NSInteger)arity;
- (NSInteger)getParamAddress:(NSInteger)pos mode:(Mode)mode;
@end

@implementation VM

- (instancetype)initWithFile:(NSString *)filename {
    if (self = [super init]) {
        self.code = [NSMutableDictionary dictionary];
        self.ip = 0;
        self.relativeBase = 0;
        self.input = [NSMutableArray array];
        self.output = [NSMutableArray array];
        [self load:filename];
    }
    return self;
}

- (void)load:(NSString *)filename {
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString *> *listStr = [content componentsSeparatedByString:@","];
    for (NSInteger i = 0; i < listStr.count; i++) {
        self.code[@(i)] = @([listStr[i] integerValue]);
    }
}

- (void)run {
    while (true) {
        Cmd *cmd = [Cmd new];
        cmd.value = [self.code[@(self.ip)] integerValue];
        
        NSInteger arity;
        switch ([cmd opCode]) {
            case 1: arity = 3; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    self.code[params[2]] = @(self.code[params[0]].integerValue + self.code[params[1]].integerValue);
                }
                break;
            case 2: arity = 3; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    self.code[params[2]] = @(self.code[params[0]].integerValue * self.code[params[1]].integerValue);
                }
                break;
            case 3: arity = 1; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    self.code[params[0]] = self.input.firstObject;
                    [self.input removeObjectAtIndex:0];
                }
                break;
            case 4: arity = 1; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    [self.output addObject:self.code[params[0]]];
                }
                break;
            case 5: arity = 2; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    if (self.code[params[0]].integerValue != 0) {
                        self.ip = self.code[params[1]].integerValue;
                        continue;
                    }
                }
                break;
            case 6: arity = 2; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    if (self.code[params[0]].integerValue == 0) {
                        self.ip = self.code[params[1]].integerValue;
                        continue;
                    }
                }
                break;
            case 7: arity = 3; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    self.code[params[2]] = @(self.code[params[0]].integerValue < self.code[params[1]].integerValue ? 1 : 0);
                }
                break;
            case 8: arity = 3; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    self.code[params[2]] = @(self.code[params[0]].integerValue == self.code[params[1]].integerValue ? 1 : 0);
                }
                break;
            case 9: arity = 1; 
                {
                    NSArray<NSNumber *> *params = [self getParamsAddresses:self.ip cmd:cmd arity:arity];
                    self.relativeBase += self.code[params[0]].integerValue;
                }
                break;
            case 99: return;
            default: return;
        }
        self.ip += arity + 1;
    }
}

- (NSArray<NSNumber *> *)getParamsAddresses:(NSInteger)pos cmd:(Cmd *)cmd arity:(NSInteger)arity {
    NSArray<NSNumber *> *modes = [cmd modesWithArity:arity];
    NSMutableArray<NSNumber *> *results = [NSMutableArray arrayWithCapacity:arity];
    for (NSInteger i = 0; i < arity; i++) {
        [results addObject:@([self getParamAddress:pos + i + 1 mode:(Mode)modes[i].integerValue])];
    }
    return results;
}

- (NSInteger)getParamAddress:(NSInteger)pos mode:(Mode)mode {
    switch (mode) {
        case Position: return [self.code[@(pos)] integerValue];
        case Immediate: return pos;
        case Relative: return self.relativeBase + [self.code[@(pos)] integerValue];
    }
    return -1;
}
@end

BOOL Beam(NSInteger x, NSInteger y) {
    VM *vm = [[VM alloc] initWithFile:@"input.txt"];
    [vm.input addObject:@(x)];
    [vm.input addObject:@(y)];
    [vm run];
    return [vm.output.lastObject integerValue] == 1;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSInteger y = 20, x = 0;
        while (true) {
            if (!Beam(x, y)) { x++; continue; }
            if (!Beam(x + 99, y)) { y++; continue; }
            if (!Beam(x, y + 99)) { x++; continue; }
            NSLog(@"%ld", (long)(x * 10000 + y));
            break;
        }
    }
    return 0;
}