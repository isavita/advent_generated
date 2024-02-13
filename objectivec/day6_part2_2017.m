#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *banks = [input componentsSeparatedByString:@"\t"];
        NSMutableArray *memoryBanks = [NSMutableArray arrayWithArray:banks];
        
        NSMutableDictionary *seenConfigurations = [NSMutableDictionary dictionary];
        NSUInteger redistributionCycles = 0;
        NSUInteger loopSize = 0;
        
        while (true) {
            redistributionCycles++;
            NSUInteger maxBlocks = 0;
            NSUInteger maxIndex = 0;
            
            for (int i = 0; i < memoryBanks.count; i++) {
                NSUInteger blocks = [memoryBanks[i] integerValue];
                if (blocks > maxBlocks) {
                    maxBlocks = blocks;
                    maxIndex = i;
                }
            }
            
            NSUInteger blocksToRedistribute = [memoryBanks[maxIndex] integerValue];
            memoryBanks[maxIndex] = @"0";
            
            NSUInteger currentIndex = (maxIndex + 1) % memoryBanks.count;
            while (blocksToRedistribute > 0) {
                memoryBanks[currentIndex] = [NSString stringWithFormat:@"%ld", [memoryBanks[currentIndex] integerValue] + 1];
                blocksToRedistribute--;
                currentIndex = (currentIndex + 1) % memoryBanks.count;
            }
            
            NSString *memoryBanksString = [memoryBanks componentsJoinedByString:@"\t"];
            if (seenConfigurations[memoryBanksString]) {
                loopSize = redistributionCycles - [seenConfigurations[memoryBanksString] integerValue];
                break;
            }
            
            seenConfigurations[memoryBanksString] = @(redistributionCycles);
        }
        
        printf("Part One: %lu\n", redistributionCycles);
        printf("Part Two: %lu\n", loopSize);
    }
    return 0;
}