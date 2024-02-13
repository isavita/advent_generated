#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSDictionary *mfcsam = @{
            @"children": @3, @"cats": @7, @"samoyeds": @2, @"pomeranians": @3,
            @"akitas": @0, @"vizslas": @0, @"goldfish": @5, @"trees": @3,
            @"cars": @2, @"perfumes": @1
        };
        
        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" "];
            NSString *sueNumber = [[parts[1] substringToIndex:[parts[1] length]-1] mutableCopy];
            
            BOOL matches = YES;
            for (int i = 2; i < [parts count]; i += 2) {
                NSString *item = [[parts[i] substringToIndex:[parts[i] length]-1] mutableCopy];
                NSNumber *count = @([[parts[i+1] substringToIndex:[parts[i+1] length]-1] intValue]);
                
                if (![mfcsam[item] isEqualToNumber:count]) {
                    matches = NO;
                    break;
                }
            }
            
            if (matches) {
                printf("%s\n", [sueNumber UTF8String]);
                break;
            }
        }
    }
    return 0;
}