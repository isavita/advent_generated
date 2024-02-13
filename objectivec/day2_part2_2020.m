#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int validPasswordsPart1 = 0;
        int validPasswordsPart2 = 0;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            
            NSArray *components = [line componentsSeparatedByString:@" "];
            NSString *rangeString = components[0];
            NSString *letter = [components[1] substringToIndex:1];
            NSString *password = components[2];
            
            NSArray *rangeComponents = [rangeString componentsSeparatedByString:@"-"];
            int min = [rangeComponents[0] intValue];
            int max = [rangeComponents[1] intValue];
            
            int count = [[password componentsSeparatedByString:letter] count] - 1;
            
            if (count >= min && count <= max) {
                validPasswordsPart1++;
            }
            
            if (([password characterAtIndex:(min - 1)] == [letter characterAtIndex:0]) ^ ([password characterAtIndex:(max - 1)] == [letter characterAtIndex:0])) {
                validPasswordsPart2++;
            }
        }
        
        printf("Part 1 Valid Passwords: %d\n", validPasswordsPart1);
        printf("Part 2 Valid Passwords: %d\n", validPasswordsPart2);
    }
    return 0;
}