#import <Foundation/Foundation.h>

@interface AssembunnyVM : NSObject
@property (nonatomic) int a;
@property (nonatomic) int b;
@property (nonatomic) int c;
@property (nonatomic) int d;
- (void)executeInstructions:(NSArray<NSString *> *)instructions;
@end

@implementation AssembunnyVM

- (instancetype)init {
    self = [super init];
    if (self) {
        _a = 0;
        _b = 0;
        _c = 0;
        _d = 0;
    }
    return self;
}

- (void)executeInstructions:(NSArray<NSString *> *)instructions {
    int pc = 0;
    while (pc < instructions.count) {
        NSString *instruction = instructions[pc];
        NSArray<NSString *> *components = [instruction componentsSeparatedByString:@" "];
        NSString *op = components[0];

        if ([op isEqualToString:@"cpy"]) {
            NSString *x = components[1];
            NSString *y = components[2];
            int value = [self valueForRegisterOrInt:x];
            [self setValue:value forRegister:y];
        } else if ([op isEqualToString:@"inc"]) {
            NSString *x = components[1];
            [self incrementRegister:x];
        } else if ([op isEqualToString:@"dec"]) {
            NSString *x = components[1];
            [self decrementRegister:x];
        } else if ([op isEqualToString:@"jnz"]) {
            NSString *x = components[1];
            NSString *y = components[2];
            int valueX = [self valueForRegisterOrInt:x];
            int valueY = [self valueForRegisterOrInt:y];
            if (valueX != 0) {
                pc += valueY - 1; // -1 because we will increment pc at the end of the loop
            }
        }
        pc++;
    }
}

- (int)valueForRegisterOrInt:(NSString *)x {
    if ([x isEqualToString:@"a"]) {
        return self.a;
    } else if ([x isEqualToString:@"b"]) {
        return self.b;
    } else if ([x isEqualToString:@"c"]) {
        return self.c;
    } else if ([x isEqualToString:@"d"]) {
        return self.d;
    } else {
        return [x intValue];
    }
}

- (void)setValue:(int)value forRegister:(NSString *)registerName {
    if ([registerName isEqualToString:@"a"]) {
        self.a = value;
    } else if ([registerName isEqualToString:@"b"]) {
        self.b = value;
    } else if ([registerName isEqualToString:@"c"]) {
        self.c = value;
    } else if ([registerName isEqualToString:@"d"]) {
        self.d = value;
    }
}

- (void)incrementRegister:(NSString *)registerName {
    if ([registerName isEqualToString:@"a"]) {
        self.a++;
    } else if ([registerName isEqualToString:@"b"]) {
        self.b++;
    } else if ([registerName isEqualToString:@"c"]) {
        self.c++;
    } else if ([registerName isEqualToString:@"d"]) {
        self.d++;
    }
}

- (void)decrementRegister:(NSString *)registerName {
    if ([registerName isEqualToString:@"a"]) {
        self.a--;
    } else if ([registerName isEqualToString:@"b"]) {
        self.b--;
    } else if ([registerName isEqualToString:@"c"]) {
        self.c--;
    } else if ([registerName isEqualToString:@"d"]) {
        self.d--;
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *instructions = [fileContents componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];

        AssembunnyVM *vm = [[AssembunnyVM alloc] init];
        [vm executeInstructions:instructions];
        NSLog(@"Value in register a: %d", vm.a);

        // Part Two
        vm = [[AssembunnyVM alloc] init];
        vm.c = 1;
        [vm executeInstructions:instructions];
        NSLog(@"Value in register a with c initialized to 1: %d", vm.a);
    }
    return 0;
}