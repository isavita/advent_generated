#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int frequency = 0;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            
            int change = [line intValue];
            frequency += change;
        }
        
        printf("%d\n", frequency);
    }
    
    return 0;
}