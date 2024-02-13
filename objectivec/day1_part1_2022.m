#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSFileHandle *inputFile = [NSFileHandle fileHandleForReadingAtPath:@"input.txt"];
        NSData *inputData = [inputFile readDataToEndOfFile];
        NSString *inputString = [[NSString alloc] initWithData:inputData encoding:NSUTF8StringEncoding];
        NSArray *lines = [inputString componentsSeparatedByString:@"\n"];
        
        int maxCalories = 0;
        int currentCalories = 0;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                if (currentCalories > maxCalories) {
                    maxCalories = currentCalories;
                }
                currentCalories = 0;
                continue;
            }
            
            int calories = [line intValue];
            currentCalories += calories;
        }
        
        if (currentCalories > maxCalories) {
            maxCalories = currentCalories;
        }
        
        printf("%d\n", maxCalories);
    }
    return 0;
}