#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *blocks = [input componentsSeparatedByString:@"\t"];
        
        NSMutableArray *memoryBanks = [NSMutableArray arrayWithArray:blocks];
        NSMutableDictionary *seenConfigurations = [NSMutableDictionary dictionary];
        
        int cycles = 0;
        
        while (1) {
            NSString *config = [memoryBanks componentsJoinedByString:@"-"];
            
            if (seenConfigurations[config]) {
                break;
            }
            
            seenConfigurations[config] = @(cycles);
            
            NSInteger maxBlocks = [[memoryBanks valueForKeyPath:@"@max.intValue"] integerValue];
            NSInteger maxIndex = [memoryBanks indexOfObject:@(maxBlocks).stringValue];
            
            memoryBanks[maxIndex] = @"0";
            
            for (int i = 1; i <= maxBlocks; i++) {
                NSInteger currentIndex = (maxIndex + i) % memoryBanks.count;
                memoryBanks[currentIndex] = @([memoryBanks[currentIndex] integerValue] + 1).stringValue;
            }
            
            cycles++;
        }
        
        printf("%d\n", cycles);
    }
    return 0;
}