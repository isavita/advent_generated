#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *intcodeArray = [input componentsSeparatedByString:@","];
        NSMutableArray *intcode = [NSMutableArray arrayWithArray:intcodeArray];
        
        int i = 0;
        while (i < intcode.count) {
            NSString *opcode = intcode[i];
            NSString *instruction = [@"00000" stringByAppendingString:opcode];
            instruction = [instruction substringFromIndex:instruction.length - 5];
            int op = [[instruction substringFromIndex:instruction.length - 2] intValue];
            
            if (op == 99) {
                break;
            }
            
            int mode1 = [[instruction substringWithRange:NSMakeRange(2, 1)] intValue];
            int mode2 = [[instruction substringWithRange:NSMakeRange(1, 1)] intValue];
            int mode3 = [[instruction substringWithRange:NSMakeRange(0, 1)] intValue];
            
            int param1 = (mode1 == 0) ? [intcode[[intcode[i + 1] intValue]] intValue] : [intcode[i + 1] intValue];
            int param2 = (mode2 == 0) ? [intcode[[intcode[i + 2] intValue]] intValue] : [intcode[i + 2] intValue];
            int param3 = [intcode[i + 3] intValue];
            
            if (op == 1) {
                int sum = param1 + param2;
                intcode[param3] = [NSString stringWithFormat:@"%d", sum];
                i += 4;
            } else if (op == 2) {
                int product = param1 * param2;
                intcode[param3] = [NSString stringWithFormat:@"%d", product];
                i += 4;
            } else if (op == 3) {
                int input = 5; // System ID for thermal radiator controller
                intcode[[intcode[i + 1] intValue]] = [NSString stringWithFormat:@"%d", input];
                i += 2;
            } else if (op == 4) {
                printf("%d\n", param1);
                i += 2;
            } else if (op == 5) {
                i = (param1 != 0) ? param2 : i + 3;
            } else if (op == 6) {
                i = (param1 == 0) ? param2 : i + 3;
            } else if (op == 7) {
                intcode[param3] = (param1 < param2) ? @"1" : @"0";
                i += 4;
            } else if (op == 8) {
                intcode[param3] = (param1 == param2) ? @"1" : @"0";
                i += 4;
            }
        }
    }
    return 0;
}