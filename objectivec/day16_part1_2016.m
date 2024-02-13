#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        
        NSString *data = input;
        while (data.length < 272) {
            NSString *copy = [data copy];
            NSMutableString *reversed = [NSMutableString stringWithString:@""];
            for (int i = (int)copy.length - 1; i >= 0; i--) {
                if ([copy characterAtIndex:i] == '0') {
                    [reversed appendString:@"1"];
                } else {
                    [reversed appendString:@"0"];
                }
            }
            data = [NSString stringWithFormat:@"%@0%@", data, reversed];
        }
        
        data = [data substringToIndex:272];
        
        NSMutableString *checksum = [NSMutableString stringWithString:data];
        while (checksum.length % 2 == 0) {
            NSMutableString *newChecksum = [NSMutableString stringWithString:@""];
            for (int i = 0; i < checksum.length; i += 2) {
                if ([checksum characterAtIndex:i] == [checksum characterAtIndex:i + 1]) {
                    [newChecksum appendString:@"1"];
                } else {
                    [newChecksum appendString:@"0"];
                }
            }
            checksum = newChecksum;
        }
        
        printf("%s\n", [checksum UTF8String]);
    }
    return 0;
}