
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *rules = [NSMutableDictionary dictionary];
        NSString *initialState = [lines[0] componentsSeparatedByString:@": "][1];
        
        for (int i = 2; i < lines.count; i++) {
            if ([lines[i] isEqualToString:@""]) {
                break;
            }
            
            NSArray *ruleComponents = [lines[i] componentsSeparatedByString:@" => "];
            rules[ruleComponents[0]] = ruleComponents[1];
        }
        
        int generations = 20;
        int pots = 500;
        int sum = 0;
        
        NSMutableArray *currentGeneration = [NSMutableArray arrayWithCapacity:pots];
        NSMutableArray *nextGeneration = [NSMutableArray arrayWithCapacity:pots];
        
        for (int i = 0; i < pots; i++) {
            currentGeneration[i] = @"."; // Empty pots
            nextGeneration[i] = @".";
        }
        
        for (int i = 0; i < initialState.length; i++) {
            currentGeneration[pots / 2 + i] = [initialState substringWithRange:NSMakeRange(i, 1)];
        }
        
        for (int gen = 0; gen < generations; gen++) {
            for (int i = 2; i < pots - 2; i++) {
                NSString *pattern = [NSString stringWithFormat:@"%@%@%@%@%@", currentGeneration[i - 2], currentGeneration[i - 1], currentGeneration[i], currentGeneration[i + 1], currentGeneration[i + 2]];
                
                if (rules[pattern]) {
                    nextGeneration[i] = rules[pattern];
                } else {
                    nextGeneration[i] = @".";
                }
            }
            
            currentGeneration = [nextGeneration mutableCopy];
        }
        
        for (int i = 0; i < pots; i++) {
            if ([currentGeneration[i] isEqualToString:@"#"]) {
                sum += i - pots / 2;
            }
        }
        
        printf("Sum of the numbers of all pots which contain a plant after 20 generations: %d\n", sum);
    }
    return 0;
}
