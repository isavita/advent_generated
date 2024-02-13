#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int count = 0;
        
        for (NSString *line in lines) {
            NSArray *sections = [line componentsSeparatedByString:@","];
            NSArray *range1 = [sections[0] componentsSeparatedByString:@"-"];
            NSArray *range2 = [sections[1] componentsSeparatedByString:@"-"];
            
            int start1 = [range1[0] intValue];
            int end1 = [range1[1] intValue];
            int start2 = [range2[0] intValue];
            int end2 = [range2[1] intValue];
            
            if ((start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)) {
                count++;
            }
        }
        
        printf("%d\n", count);
    }
    return 0;
}