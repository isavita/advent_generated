#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *expenses = [fileContents componentsSeparatedByString:@"\n"];
        
        for (NSString *expense1 in expenses) {
            for (NSString *expense2 in expenses) {
                if ([expense1 intValue] + [expense2 intValue] == 2020) {
                    printf("%d\n", [expense1 intValue] * [expense2 intValue]);
                    return 0;
                }
            }
        }
    }
    return 0;
}