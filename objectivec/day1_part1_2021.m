#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSFileHandle *file = [NSFileHandle fileHandleForReadingAtPath:@"input.txt"];
        NSData *data = [file readDataToEndOfFile];
        NSString *contents = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        
        NSArray *lines = [contents componentsSeparatedByString:@"\n"];
        
        int prev = 0;
        int current = 0;
        int count = 0;
        
        for (NSString *line in lines) {
            current = [line intValue];
            if (prev != 0 && current > prev) {
                count++;
            }
            prev = current;
        }
        
        printf("%d\n", count);
    }
    
    return 0;
}