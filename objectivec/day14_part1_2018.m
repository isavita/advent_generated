#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        int numRecipes = [input intValue];
        NSMutableArray *scoreboard = [NSMutableArray arrayWithObjects:@3, @7, nil];
        int elf1Index = 0;
        int elf2Index = 1;
        
        while ([scoreboard count] < numRecipes + 10) {
            int sum = [scoreboard[elf1Index] intValue] + [scoreboard[elf2Index] intValue];
            if (sum >= 10) {
                [scoreboard addObject:@(sum / 10)];
            }
            [scoreboard addObject:@(sum % 10)];
            
            elf1Index = (elf1Index + 1 + [scoreboard[elf1Index] intValue]) % [scoreboard count];
            elf2Index = (elf2Index + 1 + [scoreboard[elf2Index] intValue]) % [scoreboard count];
        }
        
        NSMutableString *result = [NSMutableString string];
        for (int i = numRecipes; i < numRecipes + 10; i++) {
            [result appendFormat:@"%@", scoreboard[i]];
        }
        
        printf("%s\n", [result UTF8String]);
    }
    
    return 0;
}