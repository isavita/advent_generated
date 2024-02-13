#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (!file) {
            NSLog(@"Error opening file");
            return 1;
        }
        
        char input[1024];
        fgets(input, sizeof(input), file);
        NSString *inputString = [NSString stringWithUTF8String:input];
        NSString *trimmedInput = [inputString stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        
        NSMutableArray *repeatedInput = [[NSMutableArray alloc] init];
        for (int i = 0; i < 10000; i++) {
            for (int j = 0; j < [trimmedInput length]; j++) {
                NSString *digitString = [trimmedInput substringWithRange:NSMakeRange(j, 1)];
                int digit = [digitString intValue];
                [repeatedInput addObject:[NSNumber numberWithInt:digit]];
            }
        }
        
        NSString *offsetString = [trimmedInput substringToIndex:7];
        int offset = [offsetString intValue];
        
        for (int phase = 0; phase < 100; phase++) {
            int sum = 0;
            for (int i = (int)[repeatedInput count] - 1; i >= offset; i--) {
                sum += [[repeatedInput objectAtIndex:i] intValue];
                [repeatedInput replaceObjectAtIndex:i withObject:[NSNumber numberWithInt:(sum % 10)]];
            }
        }
        
        for (int i = offset; i < offset + 8; i++) {
            printf("%d", [[repeatedInput objectAtIndex:i] intValue]);
        }
        printf("\n");
        
        fclose(file);
    }
    return 0;
}