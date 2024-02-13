#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        int ipBind = 0;
        sscanf([lines[0] UTF8String], "#ip %d", &ipBind);
        
        NSMutableArray<NSArray<NSString *> *> *instructions = [NSMutableArray array];
        for (int i = 1; i < lines.count; i++) {
            [instructions addObject:[lines[i] componentsSeparatedByString:@" "]];
        }
        
        int registers[6] = {0};
        for (int ip = 0; ip < instructions.count; ip++) {
            registers[ipBind] = ip;
            NSArray<NSString *> *inst = instructions[ip];
            NSString *opcode = inst[0];
            int a = [inst[1] intValue];
            int b = [inst[2] intValue];
            int c = [inst[3] intValue];
            
            if ([opcode isEqualToString:@"addr"]) {
                registers[c] = registers[a] + registers[b];
            } else if ([opcode isEqualToString:@"addi"]) {
                registers[c] = registers[a] + b;
            } else if ([opcode isEqualToString:@"mulr"]) {
                registers[c] = registers[a] * registers[b];
            } else if ([opcode isEqualToString:@"muli"]) {
                registers[c] = registers[a] * b;
            } else if ([opcode isEqualToString:@"banr"]) {
                registers[c] = registers[a] & registers[b];
            } else if ([opcode isEqualToString:@"bani"]) {
                registers[c] = registers[a] & b;
            } else if ([opcode isEqualToString:@"borr"]) {
                registers[c] = registers[a] | registers[b];
            } else if ([opcode isEqualToString:@"bori"]) {
                registers[c] = registers[a] | b;
            } else if ([opcode isEqualToString:@"setr"]) {
                registers[c] = registers[a];
            } else if ([opcode isEqualToString:@"seti"]) {
                registers[c] = a;
            } else if ([opcode isEqualToString:@"gtir"]) {
                registers[c] = (a > registers[b]) ? 1 : 0;
            } else if ([opcode isEqualToString:@"gtri"]) {
                registers[c] = (registers[a] > b) ? 1 : 0;
            } else if ([opcode isEqualToString:@"gtrr"]) {
                registers[c] = (registers[a] > registers[b]) ? 1 : 0;
            } else if ([opcode isEqualToString:@"eqir"]) {
                registers[c] = (a == registers[b]) ? 1 : 0;
            } else if ([opcode isEqualToString:@"eqri"]) {
                registers[c] = (registers[a] == b) ? 1 : 0;
            } else if ([opcode isEqualToString:@"eqrr"]) {
                registers[c] = (registers[a] == registers[b]) ? 1 : 0;
            }
            
            ip = registers[ipBind];
            if (ip < 0 || ip >= instructions.count) {
                break;
            }
        }
        
        printf("%d\n", registers[0]);
    }
    return 0;
}