#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *intcodeArray = [fileContents componentsSeparatedByString:@","];
        NSMutableArray *memory = [NSMutableArray arrayWithArray:intcodeArray];
        
        // Part One
        memory[1] = @12;
        memory[2] = @2;
        
        for (int i = 0; i < memory.count; i += 4) {
            if ([memory[i] intValue] == 99) {
                break;
            }
            
            int opcode = [memory[i] intValue];
            int pos1 = [memory[i + 1] intValue];
            int pos2 = [memory[i + 2] intValue];
            int pos3 = [memory[i + 3] intValue];
            
            if (opcode == 1) {
                memory[pos3] = @([memory[pos1] intValue] + [memory[pos2] intValue]);
            } else if (opcode == 2) {
                memory[pos3] = @([memory[pos1] intValue] * [memory[pos2] intValue]);
            }
        }
        
        printf("Part One - Value at position 0: %d\n", [memory[0] intValue]);
        
        // Part Two
        int targetOutput = 19690720;
        int noun = 0;
        int verb = 0;
        
        for (noun = 0; noun <= 99; noun++) {
            for (verb = 0; verb <= 99; verb++) {
                NSMutableArray *tempMemory = [NSMutableArray arrayWithArray:intcodeArray];
                tempMemory[1] = @(noun);
                tempMemory[2] = @(verb);
                
                for (int i = 0; i < tempMemory.count; i += 4) {
                    if ([tempMemory[i] intValue] == 99) {
                        break;
                    }
                    
                    int opcode = [tempMemory[i] intValue];
                    int pos1 = [tempMemory[i + 1] intValue];
                    int pos2 = [tempMemory[i + 2] intValue];
                    int pos3 = [tempMemory[i + 3] intValue];
                    
                    if (opcode == 1) {
                        tempMemory[pos3] = @([tempMemory[pos1] intValue] + [tempMemory[pos2] intValue]);
                    } else if (opcode == 2) {
                        tempMemory[pos3] = @([tempMemory[pos1] intValue] * [tempMemory[pos2] intValue]);
                    }
                }
                
                if ([tempMemory[0] intValue] == targetOutput) {
                    printf("Part Two - Noun: %d, Verb: %d, Answer: %d\n", noun, verb, 100 * noun + verb);
                    break;
                }
            }
        }
    }
    return 0;
}