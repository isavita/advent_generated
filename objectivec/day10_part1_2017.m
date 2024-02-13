#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lengths = [input componentsSeparatedByString:@","];
        
        NSMutableArray *list = [NSMutableArray array];
        for (int i = 0; i < 256; i++) {
            [list addObject:@(i)];
        }
        
        int currentPosition = 0;
        int skipSize = 0;
        
        for (NSString *length in lengths) {
            int len = [length intValue];
            NSMutableArray *sublist = [NSMutableArray array];
            for (int i = 0; i < len; i++) {
                [sublist addObject:list[(currentPosition + i) % 256]];
            }
            for (int i = 0; i < len; i++) {
                list[(currentPosition + i) % 256] = sublist[len - i - 1];
            }
            
            currentPosition = (currentPosition + len + skipSize) % 256;
            skipSize++;
        }
        
        printf("%d\n", [list[0] intValue] * [list[1] intValue]);
    }
    return 0;
}