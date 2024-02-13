#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *registers = [@{@"a": @0, @"b": @0, @"c": @0, @"d": @0} mutableCopy];
        int index = 0;
        
        while (index < instructions.count) {
            NSString *instruction = instructions[index];
            NSArray *components = [instruction componentsSeparatedByString:@" "];
            
            if ([components[0] isEqualToString:@"cpy"]) {
                NSString *value = components[1];
                NSString *registerName = components[2];
                
                if ([value rangeOfCharacterFromSet:[NSCharacterSet letterCharacterSet]].location != NSNotFound) {
                    registers[registerName] = registers[value];
                } else {
                    registers[registerName] = @([value intValue]);
                }
            } else if ([components[0] isEqualToString:@"inc"]) {
                NSString *registerName = components[1];
                registers[registerName] = @([registers[registerName] intValue] + 1);
            } else if ([components[0] isEqualToString:@"dec"]) {
                NSString *registerName = components[1];
                registers[registerName] = @([registers[registerName] intValue] - 1);
            } else if ([components[0] isEqualToString:@"jnz"]) {
                NSString *value = components[1];
                NSString *offset = components[2];
                
                int intValue = [value intValue];
                int offsetValue = [offset intValue];
                
                if (([value rangeOfCharacterFromSet:[NSCharacterSet letterCharacterSet]].location != NSNotFound && [registers[value] intValue] != 0) || intValue != 0) {
                    index += offsetValue;
                    continue;
                }
            }
            
            index++;
        }
        
        printf("Value left in register a: %d\n", [registers[@"a"] intValue]);
    }
    return 0;
}