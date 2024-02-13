#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSFileHandle *fileHandle = [NSFileHandle fileHandleForReadingAtPath:@"input.txt"];
        NSData *data = [fileHandle readDataToEndOfFile];
        NSString *input = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *registers = [[NSMutableDictionary alloc] init];
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            
            NSArray *parts = [line componentsSeparatedByString:@" "];
            NSString *reg = parts[0];
            NSString *op = parts[1];
            int amount = [parts[2] intValue];
            NSString *condReg = parts[4];
            NSString *condOp = parts[5];
            int condVal = [parts[6] intValue];
            
            BOOL cond = NO;
            if ([condOp isEqualToString:@">"]) {
                cond = [registers[condReg] intValue] > condVal;
            } else if ([condOp isEqualToString:@">="]) {
                cond = [registers[condReg] intValue] >= condVal;
            } else if ([condOp isEqualToString:@"<"]) {
                cond = [registers[condReg] intValue] < condVal;
            } else if ([condOp isEqualToString:@"<="]) {
                cond = [registers[condReg] intValue] <= condVal;
            } else if ([condOp isEqualToString:@"=="]) {
                cond = [registers[condReg] intValue] == condVal;
            } else if ([condOp isEqualToString:@"!="]) {
                cond = [registers[condReg] intValue] != condVal;
            }
            
            if (cond) {
                if ([op isEqualToString:@"inc"]) {
                    registers[reg] = @([registers[reg] intValue] + amount);
                } else if ([op isEqualToString:@"dec"]) {
                    registers[reg] = @([registers[reg] intValue] - amount);
                }
            }
        }
        
        int maxValue = 0;
        for (NSNumber *value in [registers allValues]) {
            if ([value intValue] > maxValue) {
                maxValue = [value intValue];
            }
        }
        
        printf("%d\n", maxValue);
    }
    return 0;
}