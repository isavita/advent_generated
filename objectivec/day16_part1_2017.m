
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        NSArray *moves = [input componentsSeparatedByString:@","];
        NSMutableArray *programs = [NSMutableArray arrayWithArray:@[@"a", @"b", @"c", @"d", @"e", @"f", @"g", @"h", @"i", @"j", @"k", @"l", @"m", @"n", @"o", @"p"]];
        
        for (NSString *move in moves) {
            if ([move hasPrefix:@"s"]) {
                int size = [[move substringFromIndex:1] intValue];
                NSArray *end = [programs subarrayWithRange:NSMakeRange(16 - size, size)];
                NSArray *start = [programs subarrayWithRange:NSMakeRange(0, 16 - size)];
                [programs setArray:[end arrayByAddingObjectsFromArray:start]];
            } else if ([move hasPrefix:@"x"]) {
                NSArray *positions = [[move substringFromIndex:1] componentsSeparatedByString:@"/"];
                int posA = [positions[0] intValue];
                int posB = [positions[1] intValue];
                [programs exchangeObjectAtIndex:posA withObjectAtIndex:posB];
            } else if ([move hasPrefix:@"p"]) {
                NSArray *partners = [[move substringFromIndex:1] componentsSeparatedByString:@"/"];
                NSUInteger indexA = [programs indexOfObject:partners[0]];
                NSUInteger indexB = [programs indexOfObject:partners[1]];
                [programs exchangeObjectAtIndex:indexA withObjectAtIndex:indexB];
            }
        }
        
        printf("%s\n", [[programs componentsJoinedByString:@""] UTF8String]);
    }
    return 0;
}
