#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *registers = [NSMutableDictionary dictionary];
        long long lastSound = 0;
        long long currentInstruction = 0;
        
        while (currentInstruction >= 0 && currentInstruction < instructions.count) {
            NSString *instruction = instructions[currentInstruction];
            NSArray *components = [instruction componentsSeparatedByString:@" "];
            NSString *operation = components[0];
            NSString *X = components[1];
            long long valueX = 0;
            if ([X characterAtIndex:0] >= 'a' && [X characterAtIndex:0] <= 'z') {
                if (registers[X] == nil) {
                    registers[X] = @0;
                }
                valueX = [registers[X] longLongValue];
            } else {
                valueX = [X longLongValue];
            }
            
            if ([operation isEqualToString:@"snd"]) {
                lastSound = valueX;
            } else if ([operation isEqualToString:@"set"]) {
                NSString *Y = components[2];
                long long valueY = 0;
                if ([Y characterAtIndex:0] >= 'a' && [Y characterAtIndex:0] <= 'z') {
                    if (registers[Y] == nil) {
                        registers[Y] = @0;
                    }
                    valueY = [registers[Y] longLongValue];
                } else {
                    valueY = [Y longLongValue];
                }
                registers[X] = @(valueY);
            } else if ([operation isEqualToString:@"add"]) {
                NSString *Y = components[2];
                long long valueY = 0;
                if ([Y characterAtIndex:0] >= 'a' && [Y characterAtIndex:0] <= 'z') {
                    if (registers[Y] == nil) {
                        registers[Y] = @0;
                    }
                    valueY = [registers[Y] longLongValue];
                } else {
                    valueY = [Y longLongValue];
                }
                registers[X] = @([registers[X] longLongValue] + valueY);
            } else if ([operation isEqualToString:@"mul"]) {
                NSString *Y = components[2];
                long long valueY = 0;
                if ([Y characterAtIndex:0] >= 'a' && [Y characterAtIndex:0] <= 'z') {
                    if (registers[Y] == nil) {
                        registers[Y] = @0;
                    }
                    valueY = [registers[Y] longLongValue];
                } else {
                    valueY = [Y longLongValue];
                }
                registers[X] = @([registers[X] longLongValue] * valueY);
            } else if ([operation isEqualToString:@"mod"]) {
                NSString *Y = components[2];
                long long valueY = 0;
                if ([Y characterAtIndex:0] >= 'a' && [Y characterAtIndex:0] <= 'z') {
                    if (registers[Y] == nil) {
                        registers[Y] = @0;
                    }
                    valueY = [registers[Y] longLongValue];
                } else {
                    valueY = [Y longLongValue];
                }
                registers[X] = @([registers[X] longLongValue] % valueY);
            } else if ([operation isEqualToString:@"rcv"]) {
                if (valueX != 0) {
                    printf("%lld\n", lastSound);
                    break;
                }
            } else if ([operation isEqualToString:@"jgz"]) {
                long long valueY = 0;
                if ([components[2] characterAtIndex:0] >= 'a' && [components[2] characterAtIndex:0] <= 'z') {
                    if (registers[components[2]] == nil) {
                        registers[components[2]] = @0;
                    }
                    valueY = [registers[components[2]] longLongValue];
                } else {
                    valueY = [components[2] longLongValue];
                }
                if (valueX > 0) {
                    currentInstruction += valueY - 1;
                }
            }
            
            currentInstruction++;
        }
    }
    return 0;
}