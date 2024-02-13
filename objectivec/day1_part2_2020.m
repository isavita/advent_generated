#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *expenses = [fileContents componentsSeparatedByString:@"\n"];
        
        for (int i = 0; i < expenses.count; i++) {
            for (int j = i + 1; j < expenses.count; j++) {
                if ([expenses[i] intValue] + [expenses[j] intValue] == 2020) {
                    printf("%d\n", [expenses[i] intValue] * [expenses[j] intValue]);
                }
                for (int k = j + 1; k < expenses.count; k++) {
                    if ([expenses[i] intValue] + [expenses[j] intValue] + [expenses[k] intValue] == 2020) {
                        printf("%d\n", [expenses[i] intValue] * [expenses[j] intValue] * [expenses[k] intValue]);
                    }
                }
            }
        }
    }
    return 0;
}