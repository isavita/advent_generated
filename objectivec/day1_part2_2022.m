#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSMutableArray *caloriesList = [NSMutableArray array];
        int currentCalories = 0;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                [caloriesList addObject:@(currentCalories)];
                currentCalories = 0;
                continue;
            }
            
            int calories = [line intValue];
            currentCalories += calories;
        }
        
        [caloriesList addObject:@(currentCalories)];
        NSSortDescriptor *sortDescriptor = [NSSortDescriptor sortDescriptorWithKey:@"self" ascending:NO];
        [caloriesList sortUsingDescriptors:@[sortDescriptor]];
        
        int topThreeSum = 0;
        for (int i = 0; i < 3 && i < [caloriesList count]; i++) {
            topThreeSum += [caloriesList[i] intValue];
        }
        
        printf("%d\n", topThreeSum);
    }
    return 0;
}