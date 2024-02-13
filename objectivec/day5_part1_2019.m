#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@","];
        
        NSMutableArray *memory = [instructions mutableCopy];
        
        int i = 0;
        
        while (i < memory.count) {
            NSString *instruction = memory[i];
            int opcode = [instruction intValue] % 100;
            
            if (opcode == 99) {
                break;
            }
            
            int mode1 = ([instruction intValue] / 100) % 10;
            int mode2 = ([instruction intValue] / 1000) % 10;
            
            if (opcode == 1) {
                int param1 = mode1 == 0 ? [memory[[memory[i+1] intValue]] intValue] : [memory[i+1] intValue];
                int param2 = mode2 == 0 ? [memory[[memory[i+2] intValue]] intValue] : [memory[i+2] intValue];
                int resultPos = [memory[i+3] intValue];
                
                int result = param1 + param2;
                
                memory[resultPos] = [NSString stringWithFormat:@"%d", result];
                
                i += 4;
            } else if (opcode == 2) {
                int param1 = mode1 == 0 ? [memory[[memory[i+1] intValue]] intValue] : [memory[i+1] intValue];
                int param2 = mode2 == 0 ? [memory[[memory[i+2] intValue]] intValue] : [memory[i+2] intValue];
                int resultPos = [memory[i+3] intValue];
                
                int result = param1 * param2;
                
                memory[resultPos] = [NSString stringWithFormat:@"%d", result];
                
                i += 4;
            } else if (opcode == 3) {
                int input = 1; // Provide input 1
                int resultPos = [memory[i+1] intValue];
                
                memory[resultPos] = [NSString stringWithFormat:@"%d", input];
                
                i += 2;
            } else if (opcode == 4) {
                int output = mode1 == 0 ? [memory[[memory[i+1] intValue]] intValue] : [memory[i+1] intValue];
                
                printf("%d\n", output);
                
                i += 2;
            } else {
                i++;
            }
        }
    }
    
    return 0;
}