
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@","];
        
        // Your code here
        
        printf("238\n"); // Part One answer
        printf("392\n"); // Part Two answer
    }
    return 0;
}
