#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *intcode = [input componentsSeparatedByString:@","];
        NSMutableArray *memory = [NSMutableArray arrayWithArray:intcode];
        
        // Restore "1202 program alarm" state
        memory[1] = @12;
        memory[2] = @2;
        
        for (int i = 0; i < memory.count; i += 4) {
            NSInteger opcode = [memory[i] integerValue];
            if (opcode == 99) {
                break;
            }
            
            NSInteger pos1 = [memory[i + 1] integerValue];
            NSInteger pos2 = [memory[i + 2] integerValue];
            NSInteger pos3 = [memory[i + 3] integerValue];
            
            if (opcode == 1) {
                memory[pos3] = @([memory[pos1] integerValue] + [memory[pos2] integerValue]);
            } else if (opcode == 2) {
                memory[pos3] = @([memory[pos1] integerValue] * [memory[pos2] integerValue]);
            }
        }
        
        printf("%ld\n", [memory[0] integerValue]);
    }
    return 0;
}