#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *fishAges = [input componentsSeparatedByString:@","];
        
        NSMutableArray *currentFishAges = [NSMutableArray arrayWithArray:fishAges];
        
        for (int day = 1; day <= 80; day++) {
            NSMutableArray *newFishAges = [NSMutableArray array];
            
            for (NSString *age in currentFishAges) {
                NSInteger currentAge = [age integerValue];
                if (currentAge == 0) {
                    [newFishAges addObject:@"6"];
                    [newFishAges addObject:@"8"];
                } else {
                    [newFishAges addObject:[NSString stringWithFormat:@"%ld", currentAge - 1]];
                }
            }
            
            currentFishAges = newFishAges;
        }
        
        printf("Total number of lanternfish after 80 days: %lu\n", (unsigned long)currentFishAges.count);
    }
    
    return 0;
}