#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int totalCodeLength = 0;
        int totalMemoryLength = 0;
        int totalEncodedLength = 0;
        
        for (NSString *line in lines) {
            totalCodeLength += line.length;
            
            NSMutableString *encodedLine = [@"\"" mutableCopy];
            for (int i = 0; i < line.length; i++) {
                unichar c = [line characterAtIndex:i];
                if (c == '"' || c == '\\') {
                    [encodedLine appendString:@"\\"];
                }
                [encodedLine appendFormat:@"%C", c];
            }
            [encodedLine appendString:@"\""];
            
            totalMemoryLength += [[line substringWithRange:NSMakeRange(1, line.length-2)] stringByReplacingOccurrencesOfString:@"\\\"" withString:@"\""].length;
            totalEncodedLength += encodedLine.length;
        }
        
        printf("Part One: %d\n", totalCodeLength - totalMemoryLength);
        printf("Part Two: %d\n", totalEncodedLength - totalCodeLength);
    }
    return 0;
}