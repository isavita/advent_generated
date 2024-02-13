#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSString *filePath = @"input.txt";
        NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *registers = [NSMutableDictionary dictionary];
        long highestValue = 0;
        
        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@" "];
            NSString *registerName = components[0];
            NSString *operation = components[1];
            NSInteger value = [components[2] integerValue];
            NSString *conditionRegister = components[4];
            NSString *conditionOperator = components[5];
            NSInteger conditionValue = [components[6] integerValue];
            
            if (!registers[registerName]) {
                registers[registerName] = @0;
            }
            if (!registers[conditionRegister]) {
                registers[conditionRegister] = @0;
            }
            
            BOOL conditionMet = NO;
            if ([conditionOperator isEqualToString:@">"]) {
                conditionMet = [registers[conditionRegister] integerValue] > conditionValue;
            } else if ([conditionOperator isEqualToString:@"<"]) {
                conditionMet = [registers[conditionRegister] integerValue] < conditionValue;
            } else if ([conditionOperator isEqualToString:@">="]) {
                conditionMet = [registers[conditionRegister] integerValue] >= conditionValue;
            } else if ([conditionOperator isEqualToString:@"<="]) {
                conditionMet = [registers[conditionRegister] integerValue] <= conditionValue;
            } else if ([conditionOperator isEqualToString:@"=="]) {
                conditionMet = [registers[conditionRegister] integerValue] == conditionValue;
            } else if ([conditionOperator isEqualToString:@"!="]) {
                conditionMet = [registers[conditionRegister] integerValue] != conditionValue;
            }
            
            if (conditionMet) {
                if ([operation isEqualToString:@"inc"]) {
                    registers[registerName] = @([registers[registerName] integerValue] + value);
                } else {
                    registers[registerName] = @([registers[registerName] integerValue] - value);
                }
                
                if ([registers[registerName] integerValue] > highestValue) {
                    highestValue = [registers[registerName] integerValue];
                }
            }
        }
        
        long largestValue = [[registers allValues] valueForKeyPath:@"@max.self"];
        
        printf("Part One: %ld\n", largestValue);
        printf("Part Two: %ld\n", highestValue);
    }
    return 0;
}